module NginxLint.Hint where

import Data.List
import Data.Maybe
import Debug.Trace
import qualified Text.ParserCombinators.Parsec as P

import NginxLint.Data
import NginxLint.Parse


argHints :: [Arg -> Maybe Hint]
argHints = [ hintArgExcessiveQuot
           ]

declHints :: [Decl -> Maybe Hint]
declHints = [ hintDeclPrefixRegexNoCaptures
            ]

analyzeDecl :: Decl -> [Hint]
analyzeDecl d = mapMaybe (\f -> f d) declHints
                ++ concatMap analyzeArg (extractDeclArgs d)
    where extractDeclArgs (Decl _ _ args) = args
          extractDeclArgs (Block _ _ args _) = args

analyzeArg :: Arg -> [Hint]
analyzeArg arg = mapMaybe (\f -> f arg) argHints


declIsLocation (Block _ "location" _ _) = True
declIsLocation _ = False

isLocationRegex (Block _ "location" [RawString _ op, _] _) = op `elem` ["~", "~*"]
isLocationRegex _ = False


hintDeclPrefixRegexNoCaptures :: Decl -> Maybe Hint
hintDeclPrefixRegexNoCaptures decl@(Block _ "location" [RawString _ op, RawString _ pat] _)
    | op == "~"
    && "^" `isPrefixOf` pat
    && not ("(" `isInfixOf` pat) = Just $
        Hint (ng_getPosition decl) "Loc" "prnc" ("Prefix regex location without captures: " ++ pat ++ " Use: location " ++ tail pat)
    | otherwise = Nothing
hintDeclPrefixRegexNoCaptures _ = Nothing


hintDeclIfFilename :: Decl -> Maybe Hint
hintDeclIfFilename decl@(Block _ "if" args _) = trace (show decl) Nothing
hintDeclIfFilename _ = Nothing


hintArgExcessiveQuot :: Arg -> Maybe Hint
hintArgExcessiveQuot arg@(QuotedString _ qs) =
    case P.parse plainString "" qs of
         Left _ -> Nothing
         Right (RawString _ s)
            | s == qs -> Just $ Hint (ng_getPosition arg) "Arg" "eq" ("Excessive quoting: \"" ++ qs ++ "\"")
            | otherwise -> Nothing
hintArgExcessiveQuot _ = Nothing
