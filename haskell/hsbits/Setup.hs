{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (unless)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (foldl', nub, sort)
import System.FilePath ((</>))

import Distribution.Simple hiding (installedUnitId)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Compiler
import Distribution.Text
import Distribution.InstalledPackageInfo

data LinkerOption = LinkPath FilePath
                  | LinkHs   String
                  | LinkLib  String
  deriving (Show, Eq, Ord)

main = cargoMain

cargoMain = cargoMainWithHooks simpleUserHooks

cargoMainWithHooks hooks = defaultMainWithHooks $ hooks
    { buildHook = modBuildHook $ buildHook hooks }

modBuildHook x pkg_descr localbuildinfo hooks flags = do
    cargoHook localbuildinfo
    x pkg_descr localbuildinfo hooks flags

cargoHook :: LocalBuildInfo -> IO ()
cargoHook lbi@LocalBuildInfo {buildDir} = unless (null opts) $
    writeFile (buildDir </> "cargoOpts") $ unlines opts
  where
    opts = getCargoOptions lbi

getCargoOptions :: LocalBuildInfo -> [String]
getCargoOptions lbi@LocalBuildInfo {compiler} =
    map (toCargoOption $ getCId compiler) $ getLinkerOptions lbi

toCargoOption :: String -> LinkerOption -> String
toCargoOption _   (LinkPath p) = "cargo:rustc-link-search=native=" ++ p
toCargoOption _   (LinkLib l)  = "cargo:rustc-link-lib=dylib=" ++ l
toCargoOption cId (LinkHs l)   = "cargo:rustc-link-lib=dylib=" ++ l
                                       ++ "-" ++ cId

getCId :: Compiler -> String
getCId Compiler {compilerId = CompilerId flv ver} = display flv ++ display ver

getLinkerOptions :: LocalBuildInfo -> [LinkerOption]
getLinkerOptions LocalBuildInfo { installedPkgs
                                , componentsConfigs
                                } = maybe [] allLinkerOptions $
    exactlyOne $ mapMaybe (\(_,x,_) -> getLibDepends installedPkgs x)
                          componentsConfigs

allLinkerOptions :: InstalledPackageIndex -> [LinkerOption]
allLinkerOptions = nub . sort . concatMap packageLinkerOptions . allPackages

packageLinkerOptions :: InstalledPackageInfo -> [LinkerOption]
packageLinkerOptions InstalledPackageInfo { libraryDirs
                                          , hsLibraries
                                          , extraLibraries
                                          } =
    concat [ map LinkPath libraryDirs
           , map LinkHs   hsLibraries
           , map LinkLib  extraLibraries
           ]

exactlyOne :: [a] -> Maybe a
exactlyOne [x] = Just x
exactlyOne _   = Nothing

getLibDepends :: InstalledPackageIndex -> ComponentLocalBuildInfo -> Maybe InstalledPackageIndex
getLibDepends iPkgs LibComponentLocalBuildInfo {componentPackageDeps = deps} =
    case dependencyClosure iPkgs (map fst deps) of
        Left x  -> Just x
        Right _ -> Nothing
getLibDepends _ _ = Nothing
