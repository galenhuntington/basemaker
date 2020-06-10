{-# LANGUAGE OverloadedStrings, NondecreasingIndentation #-}

--  This is what we're going to do something about.
import Prelude hiding ((.), id)
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Coerce
import Control.Exception
import Control.Monad.Trans.State.Strict
import Data.Functor
import Debug.Trace
import Control.Category
import System.IO
import System.IO.Error

import qualified Network.Wreq as Web
import qualified Network.Wreq.Session as WebS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Codec.Compression.GZip
import Data.Digest.Pure.SHA
import Lens.Micro
-- import Lens.Micro.TH
import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath
import System.Process
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Options.Applicative


--  Four tasks:
--    1.  Decide what packages to use.
--    2.  Decide what versions of those packages to use.
--    3.  Determine what modules those versions of packages contain.
--    4.  Make a prelude.


newtype Version = Version String deriving stock (Eq, Ord, Show)

--  This abstraction is becoming less and less useful.
data Package = Package { pkg :: !String, ver :: !Version }
   deriving stock (Eq, Ord, Show)
-- instance Show Package where show Package{..} = pkg ++ "-" ++ coerce ver

type Stream = BL.ByteString

{-
data Progress = Progress
   { _ignorant :: Set.Set String
   , _versioned :: Set.Set Package
   , _listed :: Set.Set (Package, [Stream])
   } deriving stock (Eq, Ord, Show)
makeLenses ''Progress
instance Semigroup Progress where
   Progress a b c <> Progress x y z = Progress (a<>x) (b<>y) (c<>z)
instance Monoid Progress where
   mempty = Progress mempty mempty mempty
-- the lens stuff was pretty, but took a different path
-}

-- data Progress = Ignorant | Versioned String | Known String [Stream]
{-
data PkInfo = PkInfo { _pkvers :: !Version, _pkmods :: !(Maybe [Stream]) }
   deriving stock (Eq, Ord, Show)
makeLenses ''PkInfo
-}

type PkInfo = (Version, Maybe [Stream])
pattern PkInfo :: Version -> Maybe [Stream] -> PkInfo
pattern PkInfo a b = (a, b)

type Progress = Maybe PkInfo
type ProgressMap = Map.Map String Progress

debug = False

log_ :: MonadIO m => String -> m ()
log_ = when debug . traceM

hackageRoot = "https://hackage.haskell.org/package/"

template name pkgs mods =
   "cabal-version:2.0\nname:" <> name <> "\nbuild-type:Simple\nversion:0\n\
   \library\n default-language:Haskell2010\n exposed-modules:Prelude\n\
   \ build-depends:" <> pkgs <> "\n\
   \ mixins:base(Prelude as Prelude.Base),base hiding(Prelude)\n\
   \ reexported-modules:" <> mods
      :: Stream

isntDone :: Progress -> Bool
isntDone (Just (PkInfo _ (Just _))) = False
isntDone _                          = True
-- isntDone = isNothing . (snd =<<)


--  Not the fanciest parser in the world.
cabalExposedModules :: Stream -> [Stream]
cabalExposedModules
   = takeWhile (\b -> isUpper (BL.head b) && ':' `BL.notElem` b)
   . drop 1
   . dropWhile
      (\w -> BL.length w /= 16 || BL.map toLower w /= "exposed-modules:")
   . filter (not . BL.null)
   . BL.splitWith (`elem` Set.fromList " \t\r\n,")

getCacheDir :: IO FilePath
getCacheDir = getUserCacheDir "basemaker"

withM :: Monad m => m a -> (a -> m b) -> m a
withM mv f = do { v <- mv; f v $> v }

--  Execute if no error encountered.
whenIO :: MonadIO m => IO a -> (a -> m ()) -> m ()
whenIO pre use = do
   x <- liftIO $ catch (Just <$> pre) (\ (_ :: IOException) -> pure Nothing)
   maybe (pure ()) use x

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mb act = maybe (pure ()) act mb

getUrl :: WebS.Session -> String -> IO Stream
getUrl web url = do
   log_ $ "Fetching " ++ url
   (^. Web.responseBody) <$> WebS.get web url

setUpCache :: IO ()
setUpCache = createDirectoryIfMissing True =<< getCacheDir

tryCache :: FilePath -> IO Stream -> IO Stream
tryCache file source = do
   path <- (</> file) <$> getCacheDir
   catch
      do BL.readFile path <* log_ ("Used cache " ++ path)
      \ (_ :: IOException) -> withM source (BL.writeFile path)

pkgString :: String -> Maybe Version -> String
pkgString pkg verm = pkg <> maybe "" (("-" <>) . coerce) verm

getCabalFile :: WebS.Session -> String -> Maybe Version -> IO Stream
getCabalFile web pkg verm = do
   let pstr = pkgString pkg verm
   (if isJust verm then tryCache (pstr <> ".cabal") else id) do
      getUrl web $ hackageRoot <> pstr </> "src" </> pkg <> ".cabal"

type Inferrer = StateT ProgressMap IO

runInferrer :: Inferrer () -> IO ProgressMap
runInferrer = flip execStateT mempty

--  Too many ad-hoc parsers.
parsePackage :: String -> (String, Maybe Version)
parsePackage s =
   (pkg, if null rest then Nothing else Just $ Version $ dropWhile (=='=') rest)
   where (pkg, rest) = break (=='=') s

parsePackages :: String -> [(String, Maybe Version)]
parsePackages = fmap parsePackage . split <=< lines where
   split [] = []
   split s  = let (a, b) = break (==',') s
              in a : split (dropWhile isSpace $ drop 1 b)

--  TODO custom filename
inferInitialSet :: Maybe String -> Inferrer ()
inferInitialSet strm = maybe (whenIO (readFile "pkgs.txt")) (&) strm $
   parsePackages >>> traverse_ \ (pkg, verm) ->
      let val = (\v -> PkInfo v Nothing) <$> verm
      in modify $ Map.alter
         (maybe (Just val) (\_ -> error $ pkg ++ " specified twice."))
         pkg

--  Another hacky parser.  Currently union with existing (initial) set.
inferSetFromRebase :: WebS.Session -> Maybe Version -> Inferrer ()
inferSetFromRebase web verm = do
   deps <- liftIO
      $ map BL.unpack
      . filter (maybe False (isAlpha . fst) . BL.uncons)
      . takeWhile (not . (':' `BL.elem`))
      . tail . dropWhile (/= "build-depends:")
      . concatMap BL.words . BL.lines
      <$> getCabalFile web "rebase" verm
   st <- get
   for_ deps \d ->
      when (d `Map.notMember` st) do
         modify $ Map.insert d Nothing

--  Uses ghc (in path) to get built-in package versions & modules.
--  Optional argument uses Stack to find built-in packages.
inferFromGhc :: Maybe (Maybe String) -> Inferrer ()
inferFromGhc resm = do
   let getPath ex args = takeWhile (/='\n') <$> readProcess ex args ""
   whenIO (
      case resm of
         Just rm -> getPath "stack" $ "path"
            : maybe [] (\r -> ["--resolver", r]) rm ++ ["--global-pkg-db"]
         _ -> getPath "ghc" ["--print-libdir"] <&> (</> "package.conf.d")
      ) \path -> do
   log_ $ "Reading " <> path
   files <- liftIO $ listDirectory path
   let list = [ (Package pkg ver, file)
         | file <- files
         , (ver', '-':pkg') <- [break (=='-') (reverse file)]
         , "fnoc." `isPrefixOf` ver'
         , let pkg = reverse pkg'
         , let ver = Version $ reverse $ drop 5 ver'  -- such elegance
         ]
   log_ $ let ver:_ = [ ver | (Package "ghc" ver, _) <- list ]
          in "Extracting from ghc-" ++ coerce ver
   st <- get
   for_ list \ (Package pkg ver, file) ->
      when (maybe False (maybe True (== PkInfo ver Nothing)) $ Map.lookup pkg st) do
         mods <- liftIO $ cabalExposedModules <$> BL.readFile (path </> file)
         modify $ Map.insert pkg $ Just (PkInfo ver $ Just mods)

inferFromHackage :: WebS.Session -> Inferrer ()
inferFromHackage web = do
   st <- get
   for_ (Map.toList st) \ (pkg, prog) -> -- Just (PkInfo ver Nothing))
      when (isntDone prog) do
         let verm = fmap fst prog
         cabal <- liftIO $ getCabalFile web pkg verm
         let mods = cabalExposedModules cabal
         let ver = flip fromMaybe verm $ go
                     $ map (BL.map toLower) $ BL.words cabal where
               go ("version:" : v : _) = Version (BL.unpack v)
               go (_ : l) = go l
               go [] = error $ "Cabal file missing version for " ++ pkg
         modify $ Map.insert pkg $ Just $ PkInfo ver $ Just mods

--  Uses a Stack snapshot to get versions.
inferFromStack :: WebS.Session -> Maybe String -> Inferrer ()
inferFromStack web resolverm = do
   resolver <- liftIO $ flip (flip maybe pure) resolverm $
      head <$> drop 1 <$> dropWhile (/="resolver:")
           <$> words <$> readFile "stack.yaml"
   conf <- liftIO $ tryCache ("stack-" <> resolver <> ".conf") do
      getUrl web $ "https://www.stackage.org/snapshot/"
                  ++ resolver ++ "/cabal.config?global=true"
   st <- get
   for_ (filter (not . ("--" `BL.isPrefixOf`)) $ BL.lines conf) \line ->
      case words (BL.unpack line) of
         _ : pkg : ver' : _
            | Map.lookup pkg st == Just Nothing
            , ver <- dropWhile (not.isDigit) ver'
            , not . null $ ver
            -> modify $ Map.insert pkg $ Just $ PkInfo (Version ver) Nothing
         _  -> pure ()

preludeFromRebase :: Set.Set Stream -> WebS.Session -> Maybe Version -> IO Stream
preludeFromRebase mods web verm = do
   let pstr = pkgString "rebase" verm
   hs <- (if isJust verm then tryCache ("prelude-" <> pstr <> ".hs") else id)
      $ getUrl web $ hackageRoot <> pstr </> "src/library/Rebase/Prelude.hs"
   pure $ BL.unlines $ "module Prelude (module X) where" : do
      imp <- filter ("import" `BL.isPrefixOf`) $ BL.lines hs
      -- compactify...
      case drop 1 $ BL.words imp of
         "Prelude" : "as" : _ : rest
               -> pure $ "import Prelude.Base as X " <> BL.unwords rest
         mod' : "as" : _ : rest
            | "Rebase." `BL.isPrefixOf` mod'
            , mod <- BL.drop 7 mod'
            , mod `Set.member` mods -- particularly List1
               -> pure $ "import " <> mod <> " as X " <> BL.unwords rest
         _     -> mempty

main = do
   let verParser :: String -> Maybe Version
       verParser "latest" = Nothing
       verParser v = Just (Version v)

   (packageSpec, preludeSpec, stackSpec, rebaseSpec, outSpec)
         <- execParser $ flip info
      (fullDesc
         <> header "basemaker - because Haskell is just too easy"
         <> progDesc
            "Make your own Haskell prelude using deep learning and the blockchain."
         )
      $ (<**> helper) $ pure (,,,,)
         <*> optional (strOption (
            long "packages" <> metavar "LIST" <> showDefault
               <> help "List of packages to use, e.g., \"base==4.14.0.0,containers\"."))
         <*> optional (strOption (
            long "prelude" <> metavar "FILE"
               <> help "Prelude file to bundle. (default: \"./Prelude.hs\")"))
         <*> optional (
            flag' Nothing (short 's' <> help "Use local Stack config to infer versions.")
            <|> fmap Just (strOption (
               long "stackage" <> metavar "RESOLVER"
                  <> help "Use RESOLVER (e.g., lts-15.15) to infer versions.")))
         <*> optional (verParser <$> strOption (
            short 'r' <> long "rebase" <> metavar "VERSION"
               <> help "Use package list and/or Prelude from Rebase; \
                       \VERSION can be literal (e.g., 1.6.1) or \"latest\"."))
         <*> strOption (
            short 'o' <> metavar "NAME" <> value "./mybase" <> showDefault
               <> help "Directory and name of generated package.")

   setUpCache
   web <- WebS.newSession
   st <- runInferrer do
      inferInitialSet packageSpec
      whenJust rebaseSpec $ inferSetFromRebase web
      whenJust stackSpec $ inferFromStack web
      inferFromGhc stackSpec
      inferFromHackage web
      when debug $ liftIO . traverse_
         (log_ . show . (_2 . mapped %~ \ (PkInfo v m) -> (v, fmap length m)))
            . Map.toList =<< get
   let (pkgs, mods) =
         unzip [
            case val of
               Just (PkInfo ver (Just mod_)) -> (Package pkg ver, mod_)
               _ -> error $ "Could not get modules for " ++ pkg
            | (pkg, val) <- Map.toList st ]
         & over _2 (Set.delete "Prelude" . Set.fromList . concat)
   let (outFile, target) =
         if ".tar.gz" `isSuffixOf` outSpec
         then (drop (length outSpec - 7) outSpec, outSpec)
         else (outSpec, outSpec <> ".tar.gz")
   let myCabal = template
         (BL.pack $ takeFileName outFile)
         (BL.pack $ intercalate "," $ map showp pkgs)
         (BL.intercalate "," $ "Prelude.Base" : Set.toList mods)
        where showp (Package pkg ver) = pkg ++ "==" ++ coerce ver
   myPrelude <- case preludeSpec of
      Just f -> BL.readFile f
      _  -> case rebaseSpec of
         Just rv -> preludeFromRebase mods web rv
         Nothing -> BL.readFile "Prelude.hs"
   let tarEntry fn = Tar.fileEntry $ let Right x = Tar.toTarPath False fn in x
   let tgz = compressWith defaultCompressParams { compressLevel = bestCompression }
         $ Tar.write
         [tarEntry "mybase.cabal" myCabal, tarEntry "Prelude.hs" myPrelude]
   let save = do
         BL.writeFile target tgz
         log_ $ "Generated " ++ target ++ "."
   handleJust (\e -> guard (isDoesNotExistError e) *> pure ()) (const save) do
      eq <- withFile target ReadMode \h ->
         BL.hGetContents h <&> (/= tgz) >>= evaluate
      if eq
         then putStrLn "Overwriting old tarball." *> save
         else putStrLn $ target ++ " already exists and is up to date."
   putStrLn $ "sha256: " <> show (sha256 tgz)

