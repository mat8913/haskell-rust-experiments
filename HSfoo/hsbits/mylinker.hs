{-# LANGUAGE CPP #-}

import System.Environment (getArgs)
import Control.Monad ((>=>), unless)
import Data.Traversable (for)
import Data.Maybe (mapMaybe)
import Data.List (stripPrefix)
import Text.Read (readMaybe)
import System.Directory (listDirectory)

#if MIN_VERSION_unix(2,7,1)
import qualified System.Posix.Process as P (executeFile)

runCc :: IO ()
runCc = getArgs >>= \a -> P.executeFile "cc" True a Nothing
#else
import qualified System.Process as P
import qualified System.Exit as E

runCc :: IO ()
runCc = do
    a <- getArgs
    let p = (P.proc "cc" a) { P.delegate_ctlc = True }
    (_,_,_,h) <- P.createProcess p
    e <- P.waitForProcess h
    E.exitWith e
#endif

libsuffix :: String
#ifdef LIBSUFFIX
libsuffix = LIBSUFFIX
#endif

wantedRts :: String
#ifdef WANTEDRTS
wantedRts = WANTEDRTS
#endif

ghcRtsDir :: FilePath
#ifdef GHCRTSDIR
ghcRtsDir = GHCRTSDIR
#endif

data LinkerOption = LinkerLib  String
                  | LinkerPath FilePath
  deriving Show

main :: IO ()
main = do
    linkerOpts <- getArgs >>= parseArgs
    unless (null linkerOpts) $ do
        cargoOpts <- map toCargoOption . (:linkerOpts) <$> rtsLinkerOption
        writeFile "dist/cargoOpts" $ unlines cargoOpts ++ "\n"
    runCc

-- rtsLinkerOption :: IO LinkerOption
rtsLinkerOption = fmap (fmap LinkerLib . chooseRts . mapMaybe determineRts)
    (listDirectory ghcRtsDir) >>= maybe (fail "Failed to find RTS.") return

toCargoOption :: LinkerOption -> String
toCargoOption (LinkerLib l)  = "cargo:rustc-link-lib=dylib=" ++ l
toCargoOption (LinkerPath p) = "cargo:rustc-link-search=native=" ++ p

parseArgs :: [String] -> IO [LinkerOption]
parseArgs xs = fmap concat . for (mapMaybe parseArg xs) $
    either (fileArgs >=> parseArgs) (pure . pure)

parseArg :: String -> Maybe (Either FilePath LinkerOption)
parseArg ('@':p)     = Just $ Left p
parseArg ('-':'l':l) = Just $ Right $ LinkerLib l
parseArg ('-':'L':p) = Just $ Right $ LinkerPath p
parseArg _           = Nothing

fileArgs :: FilePath -> IO [String]
fileArgs = fmap (mapMaybe readMaybe . lines) . readFile

determineRts :: String -> Maybe (String, String)
determineRts = fmap (break ('-'==)) .
    (stripPrefix "libHSrts" >=> stripSuffix libsuffix)

chooseRts :: [(String, String)] -> Maybe String
chooseRts [] = Nothing
chooseRts ((got, end):xs) = if wantedRts == got
    then Just $ "HSrts" ++ got ++ end
    else chooseRts xs

stripSuffix :: Eq a => [a]       -- ^ Suffix
                    -> [a]       -- ^ Full list
                    -> Maybe [a] -- ^ Prefix
stripSuffix = foldr (>=>) Just . map removeSuffix . reverse

removeSuffix :: Eq a => a         -- ^ Suffix
                     -> [a]       -- ^ Full list
                     -> Maybe [a] -- ^ Prefix
removeSuffix _ []     = Nothing
removeSuffix s [x]    = if x == s then Just [] else Nothing
removeSuffix s (x:xs) = (x:) <$> removeSuffix s xs
