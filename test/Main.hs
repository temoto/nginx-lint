module Main (main) where

import Data.Monoid (mempty)
import Debug.Trace
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import qualified Test.Framework as T
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Property as QP
import qualified Text.Parsec

import qualified NginxLint.Parse


main :: IO ()
main = do
    let empty_test_opts = mempty :: TestOptions
    let my_test_opts = empty_test_opts {
        topt_maximum_generated_tests = Just 2000
        , topt_maximum_unsuitable_generated_tests = Just 10000
        , topt_maximum_test_size = Just 500
    }
    let empty_runner_opts = mempty :: RunnerOptions
    let my_runner_opts = empty_runner_opts {
        ropt_test_options = Just my_test_opts
        , ropt_color_mode = Just T.ColorAuto
    }
    T.defaultMainWithOpts tests my_runner_opts
    where
        tests = [
            T.testGroup "Parser" $ [
                testProperty "comment-01" $ Q.forAll genComment checkFile
                , testProperty "arg-01" $ Q.forAll genArg checkArg
                , testProperty "decl-01" $ Q.forAll genRewrite checkDecl
                , testProperty "block-01" $ Q.forAll genBlock checkDecl
            ]
            ]

genArg :: Q.Gen String
genArg = Q.oneof [genPlainString]

genBlock :: Q.Gen String
genBlock = do
    name <- Q.oneof [return "events", return "http", return "server", genIdentifier]
    args <- Q.resize 3 $ Q.listOf genArg
    decls <- Q.resize 5 $ Q.listOf genDecl
    return $ name ++ " " ++ unwords args ++ " {" ++ concat decls ++ "}"

genComment :: Q.Gen String
genComment = do
    s <- Q.listOf $ Q.arbitrary `Q.suchThat` (/= '\n')
    return $ "#" ++ s

genDecl :: Q.Gen String
genDecl = Q.frequency [
    (1, genBlock)
    , (2, genDeclIf)
    , (10, genDeclSimple)
    ]

genDeclIf :: Q.Gen String
genDeclIf = do
    args <- Q.resize 3 $ Q.listOf1 genArg
    decls <- Q.resize 5 $ Q.listOf genDeclSimple
    return $ "if (" ++ unwords args ++ ") {" ++ concat decls ++ "}"

genDeclSimple :: Q.Gen String
genDeclSimple = do
    name <- genIdentifier
    args <- Q.resize 3 $ Q.listOf genArg
    return $ name ++ " " ++ unwords args ++ ";"

genIdentifier :: Q.Gen String
genIdentifier = Q.resize 20 $ Q.listOf1 $ Q.elements identChars

genPlainString :: Q.Gen String
genPlainString = Q.listOf1 $ Q.elements plainStringChars

genRegex :: Q.Gen String
genRegex = do
    prefix <- genFreqMaybe 2 begin
    suffix <- genFreqMaybe 2 end
    middle <- fmap concat $ Q.resize 10 $ Q.listOf genoptions
    return $ prefix ++ middle ++ suffix
    where
        genoptions = Q.frequency [
            (1, group)
            , (3, klass)
            , (5, raw0)
            ]
        klass = do
            m <- fmap concat $ Q.resize 5 $ Q.listOf1 klassoptions
            flag <- genFreqMaybe 3 begin
            return $ "[" ++ flag ++ m ++ "]"
        klassoptions = Q.frequency [
            (3, raw1)
            , (1, return "a-z")
            , (1, return "0-9")
            ]
        group = do
            flag <- genFreqMaybe 1 (return "?:")
            m <- fmap concat $ Q.resize 5 $ Q.listOf1 groupgenoptions
            q <- genquant
            return $ "(" ++ flag ++ m ++ ")" ++ q
        groupgenoptions = Q.frequency [
            (1, return "|")
            , (1, genEmpty)
            , (1, begin)
            , (1, end)
            , (2, group)
            , (3, klass)
            , (10, raw1)
            ]
        genquant = Q.frequency [
            (10, genEmpty)
            , (1, return "?")
            , (1, return "*")
            ]
        begin = return "^"
        end = return "$"
        raw0 = Q.resize 10 $ Q.listOf genchar
        raw1 = Q.resize 10 $ Q.listOf1 genchar
        genchar = Q.elements $ letterChars ++ digitChars ++ "~!@%&."

genRewrite :: Q.Gen String
genRewrite = do
    rule <- genRegex
    target <- gtarget
    flag <- gflag
    return $ concat ["rewrite ", rule, " ", target, flag, ";"]
    where
        gtarget = Q.elements ["/", "/$1_2x.$2", "http://example$request_uri?"]
        gflag = Q.elements ["", " break", " last", " permanent"]

-- Begin utils

genEmpty :: Q.Gen String
genEmpty = return ""

genFreqMaybe :: Int -> Q.Gen String -> Q.Gen String
genFreqMaybe n g = Q.frequency [(n, g), (10, genEmpty)]

assertParsed :: Either Text.Parsec.ParseError a -> QP.Result
assertParsed a = case a of
    (Right _) -> QP.succeeded
    (Left e) -> QP.failed { QP.expect = True, QP.reason = show e }

checkFile :: String -> QP.Property
checkFile = checkParsed NginxLint.Parse.parseFile
checkArg :: String -> QP.Property
checkArg = checkParsed NginxLint.Parse.argument
checkDecl :: String -> QP.Property
checkDecl = checkParsed NginxLint.Parse.decl

checkParsed :: Text.Parsec.Parsec [a] () a1 -> [a] -> QP.Property
checkParsed p s = Q.collect (length s) $ Q.within 100000 $ assertParsed $ Text.Parsec.parse p "" s

digitChars :: String
digitChars = ['0'..'9']
letterChars :: String
letterChars = ['A'..'Z'] ++ ['a'..'z']
identChars :: String
identChars = letterChars ++ digitChars ++ "_"
plainStringChars :: String
plainStringChars = identChars ++ "~!?@%^&*"

-- End utils
