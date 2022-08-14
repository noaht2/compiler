module Main where
import System.Environment
import System.Process
import Control.Monad
import Control.Monad.State.Strict
import Control.Category

import Types

import Parsing
import Parser

import Mangler
import Cps
import Unroll
import Asm
import Backend

compile :: String -> String
compile code = evalState (((apply_parser program_parser >>> return) >=> mangle >=> convert >=> (unroll >>> return) >=> sketch >=> backend) code) 0

repeat_calling 0 f x = x
repeat_calling n f x = repeat_calling (n - 1) f (f x)

main :: IO ()
main = do
  [source_code_filename] <- getArgs
  source_code <- fmap init (readFile source_code_filename)
  let assembly_code = compile source_code
  let assembly_code_filename = (repeat_calling 4 init source_code_filename) ++ "mms"
  writeFile assembly_code_filename assembly_code
  createProcess (shell ("mmixal -x " ++ assembly_code_filename))
  pure ()
