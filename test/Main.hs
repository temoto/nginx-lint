module Main (main) where

import Data.List (isInfixOf)
import qualified Test.Framework as T
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck as Q
import qualified Text.ParserCombinators.Parsec as P

import NginxLint.Parse (argument, parseFile)


main :: IO ()
main = T.defaultMain tests

tests =
    [
    T.testGroup "Cases" $ zipWith (testCase . ("Case "++) . show) [1::Int ..] [
    ],
    T.testGroup "Properties" $ zipWith (testProperty . ("Property "++) . show) [1::Int ..] [
        prop_ParseComment01
        , prop_ParseArg01
    ]
    ]


genComment :: Q.Gen String
genComment = do
    s <- Q.arbitrary `Q.suchThat` notElem '\n'
    return $ "#" ++ s

genPlainString :: Q.Gen String
genPlainString = Q.listOf1 c
    where
        c = Q.elements (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "~!@#$%^&*")

genArg :: Q.Gen String
genArg = Q.oneof [genPlainString]

prop_ParseComment01 = Q.forAll genComment isValidConfig

prop_ParseArg01 = Q.forAll genArg isValidArg


-- Begin utils

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isValidConfig :: String -> Bool
isValidConfig s = isRight $ P.parse parseFile "" s

isValidArg :: String -> Bool
isValidArg s = isRight $ P.parse argument "" s

-- End utils
