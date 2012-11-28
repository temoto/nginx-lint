module Main (main) where

import Data.Generics.Uniplate.Operations (universeBi)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (parseFromFile)

import NginxLint.Data (NgFile(..), ppDecl, ppHint)
import NginxLint.Hint (analyzeDecl)
import NginxLint.Parse (parseFile)


main :: IO ()
main = processFiles

processFiles :: IO ()
processFiles = do
    args <- getArgs
    if not (null args) && head args == "-print"
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
processHints f@(NgFile fname _) = doHints
    where
        doHints = if null hints
                      then putStrLn (fname ++ ": No suggestions.")
                      else mapM_ (putStrLn . ppHint) hints

        hints = concatMap analyzeDecl (universeBi f)

printParsed :: NgFile -> IO ()
printParsed (NgFile fname decls) = do
    putStrLn $ "# " ++ fname
    putStrLn $ "#"
    mapM_ (putStrLn . ppDecl) decls
