module Main where

import Language.JavaScript.Parser.AST
import Language.JavaScript.Pretty.Printer.Extended

main :: IO ()
main = putStrLn $ prettyPrint $ JSLiteral JSNoAnnot "hello"
