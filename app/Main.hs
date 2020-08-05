{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Environment ( getArgs )

import Data.Comp.Multi ( project, transform, HFunctor, Term, (:-<:), All )

import Cubix.ParsePretty

import Cubix.Language.C.Parametric.Common
import Cubix.Language.Java.Parametric.Common
import Cubix.Language.JavaScript.Parametric.Common
import Cubix.Language.Lua.Parametric.Common
import Cubix.Language.Python.Parametric.Common as PCommon

import Cubix.Language.Parametric.Syntax


vandalize :: (Ident :-<: fs, All HFunctor fs) => Term fs l -> Term fs l
vandalize t = transform vandalizeInner t
  where
    vandalizeInner :: (Ident :-<: fs, All HFunctor fs) => Term fs l -> Term fs l
    vandalizeInner (project -> Just (Ident s)) = iIdent (s ++ "_vandalized")
    vandalizeInner t                           = t

runTransform :: IO (Maybe t) -> (t -> String) -> IO ()
runTransform mParsedFile use = do
   parsedFile <- mParsedFile
   maybe (return ()) putStrLn (use <$> parsedFile)

runVandalize :: String -> FilePath -> IO ()
runVandalize "c"      filename = runTransform (parseFile @MCSig      filename) (pretty . vandalize)
runVandalize "java"   filename = runTransform (parseFile @MJavaSig   filename) (pretty . vandalize)
runVandalize "js"     filename = runTransform (parseFile @MJSSig     filename) (pretty . vandalize)
runVandalize "lua"    filename = runTransform (parseFile @MLuaSig    filename) (pretty . vandalize)
runVandalize "python" filename = runTransform (parseFile @MPythonSig filename) (pretty . vandalize)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [lang, file] -> runVandalize lang file
    _            -> putStrLn "Usage: cubix-sample-app <lang> <filename>"
