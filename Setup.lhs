#!/usr/bin/env runhaskell
\begin{code}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List ( nub )
import Distribution.Package ( PackageId, InstalledPackageId, packageVersion, packageName, unPackageName, mungedName', mungedVersion' )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks, showVersion )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Types.UnqualComponentName ( unUnqualComponentName )
import Distribution.Types.MungedPackageName ( unMungedPackageName )
import Distribution.Types.MungedPackageId ( MungedPackageId )
import Distribution.Verbosity ( Verbosity )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      let testName' = unUnqualComponentName $ testName suite
      rewriteFile (dir </> "Build_" ++ testName' ++ ".hs") $ unlines
        [ "module Build_" ++ testName' ++ " where"
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (testDeps libcfg suitecfg))
        ]
  where
    formatdeps = map (formatone . snd)
    formatone p =
      (unMungedPackageName $ mungedName' p) ++ "-" ++ showVersion (mungedVersion' p)

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, MungedPackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

\end{code}
