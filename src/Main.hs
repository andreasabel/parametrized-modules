
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import ParMod.Abs        (Program)
import ParMod.ErrM       (Err(Ok,Bad))
import ParMod.Layout     (resolveLayout)
import ParMod.Par        (pProgram, myLexer)
import ParMod.Print      (printTree)

import TypeChecker       (checkProgram)

-- | Main: read file passed by only command line argument,
--   parse and scope check.

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= parse >>= check
    _      -> do
      putStrLn "Usage: Main <SourceFile>"
      exitFailure

-- | Parse, file contents.

parse :: String -> IO Program
parse s = do
  case pProgram (resolveLayout True $ myLexer s) of
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok  prg -> return prg

-- | Type check.

check :: Program -> IO ()
check prg = do
  putStrLn $ printTree prg
  case checkProgram prg of
    Left err -> do
      putStrLn "SCOPE ERROR"
      print err
      exitFailure
    Right sig -> do
      putStrLn "SUCCESS"
      print sig
