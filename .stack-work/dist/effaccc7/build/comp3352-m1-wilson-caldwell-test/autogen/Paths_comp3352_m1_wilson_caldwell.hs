{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_comp3352_m1_wilson_caldwell (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\James\\Downloads\\comp3352-m2-wilson-caldwell\\comp3352-m1-wilson-caldwell\\.stack-work\\install\\157926f3\\bin"
libdir     = "C:\\Users\\James\\Downloads\\comp3352-m2-wilson-caldwell\\comp3352-m1-wilson-caldwell\\.stack-work\\install\\157926f3\\lib\\x86_64-windows-ghc-9.6.6\\comp3352-m1-wilson-caldwell-0.1.0.0-ANp6DMT3etABf01G3mdjG0-comp3352-m1-wilson-caldwell-test"
dynlibdir  = "C:\\Users\\James\\Downloads\\comp3352-m2-wilson-caldwell\\comp3352-m1-wilson-caldwell\\.stack-work\\install\\157926f3\\lib\\x86_64-windows-ghc-9.6.6"
datadir    = "C:\\Users\\James\\Downloads\\comp3352-m2-wilson-caldwell\\comp3352-m1-wilson-caldwell\\.stack-work\\install\\157926f3\\share\\x86_64-windows-ghc-9.6.6\\comp3352-m1-wilson-caldwell-0.1.0.0"
libexecdir = "C:\\Users\\James\\Downloads\\comp3352-m2-wilson-caldwell\\comp3352-m1-wilson-caldwell\\.stack-work\\install\\157926f3\\libexec\\x86_64-windows-ghc-9.6.6\\comp3352-m1-wilson-caldwell-0.1.0.0"
sysconfdir = "C:\\Users\\James\\Downloads\\comp3352-m2-wilson-caldwell\\comp3352-m1-wilson-caldwell\\.stack-work\\install\\157926f3\\etc"

getBinDir     = catchIO (getEnv "comp3352_m1_wilson_caldwell_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "comp3352_m1_wilson_caldwell_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "comp3352_m1_wilson_caldwell_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "comp3352_m1_wilson_caldwell_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "comp3352_m1_wilson_caldwell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "comp3352_m1_wilson_caldwell_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
