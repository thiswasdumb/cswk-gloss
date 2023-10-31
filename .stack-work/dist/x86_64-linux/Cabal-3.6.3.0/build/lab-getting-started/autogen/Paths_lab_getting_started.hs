{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_lab_getting_started (
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
bindir     = "/dcs/22/u2207514/cs141/cswk-gloss/.stack-work/install/x86_64-linux/71dd22bb4c7ef22672f6d5cb00f4271cf4c8e2b725b27b1910882771b22ae49e/9.2.5/bin"
libdir     = "/dcs/22/u2207514/cs141/cswk-gloss/.stack-work/install/x86_64-linux/71dd22bb4c7ef22672f6d5cb00f4271cf4c8e2b725b27b1910882771b22ae49e/9.2.5/lib/x86_64-linux-ghc-9.2.5/lab-getting-started-0.1.0.0-3lspEn9HGJx9LSFo37JSNv-lab-getting-started"
dynlibdir  = "/dcs/22/u2207514/cs141/cswk-gloss/.stack-work/install/x86_64-linux/71dd22bb4c7ef22672f6d5cb00f4271cf4c8e2b725b27b1910882771b22ae49e/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/dcs/22/u2207514/cs141/cswk-gloss/.stack-work/install/x86_64-linux/71dd22bb4c7ef22672f6d5cb00f4271cf4c8e2b725b27b1910882771b22ae49e/9.2.5/share/x86_64-linux-ghc-9.2.5/lab-getting-started-0.1.0.0"
libexecdir = "/dcs/22/u2207514/cs141/cswk-gloss/.stack-work/install/x86_64-linux/71dd22bb4c7ef22672f6d5cb00f4271cf4c8e2b725b27b1910882771b22ae49e/9.2.5/libexec/x86_64-linux-ghc-9.2.5/lab-getting-started-0.1.0.0"
sysconfdir = "/dcs/22/u2207514/cs141/cswk-gloss/.stack-work/install/x86_64-linux/71dd22bb4c7ef22672f6d5cb00f4271cf4c8e2b725b27b1910882771b22ae49e/9.2.5/etc"

getBinDir     = catchIO (getEnv "lab_getting_started_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "lab_getting_started_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "lab_getting_started_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "lab_getting_started_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lab_getting_started_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lab_getting_started_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
