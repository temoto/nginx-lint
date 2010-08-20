module Main where

import Data.Generics.Uniplate.Operations
import System.Environment
import System.Exit
import System.IO
import Text.ParserCombinators.Parsec (parseFromFile)

import NginxLint.Data
import NginxLint.Hint
import NginxLint.Parse


main = processFiles

processFiles :: IO ()
processFiles = do
    args <- getArgs
    if length args >= 1 && head args == "-print"
       then mapM_ (processFile printParsed) (tail args)
       else mapM_ (processFile processHints) args

processFile :: (NgFile -> IO ()) -> FilePath -> IO ()
processFile fun fname = do
    result <- parseFromFile parseFile fname
    case result of
         Left err -> do
             print err
             exitFailure
         Right ngfile -> fun ngfile

processHints :: NgFile -> IO ()
processHints f@(NgFile fname _) = do_hints
    where
        do_hints = if null hints
                      then putStrLn (fname ++ ": No suggestions.")
                      else mapM_ (putStrLn . ppHint) hints

        all_decls = universeBi f
        hints = concatMap analyzeDecl all_decls

printParsed :: NgFile -> IO ()
printParsed (NgFile fname decls) = do
    putStrLn $ "# " ++ fname
    putStrLn $ "#"
    mapM_ (putStrLn . ppDecl) decls
