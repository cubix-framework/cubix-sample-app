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

import Cubix.Language.SuiMove.IPS ( MSuiMoveSig )
import Cubix.Language.SuiMove.IPS.Trans qualified as SuiMoveTrans
import Cubix.Language.SuiMove.IPS.Types ( MSuiMoveTerm )
import Cubix.Language.SuiMove.Modularized ( SourceFileL )
import Cubix.Language.SuiMove.ParsePretty ()
import Cubix.Language.SuiMove.Pretty qualified as SuiMovePretty

-- | Append "_vandalized" to every identifier in a term.
-- Fully generic: works on any language whose IPS includes Ident.
vandalize :: (Ident :-<: fs, All HFunctor fs) => Term fs l -> Term fs l
vandalize = transform go
  where
    go :: (Ident :-<: fs, All HFunctor fs) => Term fs l -> Term fs l
    go (project -> Just (Ident s)) = iIdent (s ++ "_vandalized")
    go t                           = t

runTransform :: IO (Maybe t) -> (t -> String) -> IO ()
runTransform mParsedFile use = do
   parsedFile <- mParsedFile
   maybe (return ()) putStrLn (use <$> parsedFile)

runVandalize :: String -> FilePath -> IO ()
runVandalize "c"        filename = runTransform (parseFile @MCSig      filename) (pretty . vandalize)
runVandalize "java"     filename = runTransform (parseFile @MJavaSig   filename) (pretty . vandalize)
runVandalize "js"       filename = runTransform (parseFile @MJSSig     filename) (pretty . vandalize)
runVandalize "lua"      filename = runTransform (parseFile @MLuaSig    filename) (pretty . vandalize)
runVandalize "python"   filename = runTransform (parseFile @MPythonSig filename) (pretty . vandalize)
runVandalize "sui-move" filename = do
  mterm <- parseFile @MSuiMoveSig filename
  case mterm of
    Nothing   -> putStrLn $ "Failed to parse: " ++ filename
    Just term ->
      let vandalized = vandalize term :: MSuiMoveTerm SourceFileL
      in  putStrLn $ SuiMovePretty.pretty (SuiMoveTrans.untranslate vandalized)
runVandalize lang _ = putStrLn $ "Unknown language: " ++ lang ++ "\nSupported: c, java, js, lua, python, sui-move"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [lang, file] -> runVandalize lang file
    _            -> putStrLn "Usage: cubix-sample-app <lang> <filename>"
