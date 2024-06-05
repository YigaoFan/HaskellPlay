module ProgramLoader where

import System.IO (readFile)
import System.Directory (getDirectoryContents)
import Data.Functor ((<&>))
import GMachine.Program (exe)

load :: FilePath -> IO [IO String]
load dirName =
  getDirectoryContents dirName <&> map readFile

run = load "./ExampleProgram" <&> (<&> (<&> exe))
