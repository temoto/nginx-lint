module NginxLint.Hint (
    analyzeDecl
) where

import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (mapMaybe)
--import Debug.Trace (trace)
import qualified Text.ParserCombinators.Parsec as P

import NginxLint.Data
import NginxLint.Parse (plainString)


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
          extractDeclArgs (If _ args _) = args
          extractDeclArgs (Location _ args _) = args

analyzeArg :: Arg -> [Hint]
analyzeArg arg = mapMaybe (\f -> f arg) argHints


-- Not used yet.
--declIsLocation :: Decl -> Bool
--declIsLocation (Block _ "location" _ _) = True
--declIsLocation _ = False

--isLocationRegex :: Decl -> Bool
--isLocationRegex (Block _ "location" [RawString _ op, _] _) = op `elem` ["~", "~*"]
--isLocationRegex _ = False


hintDeclPrefixRegexNoCaptures :: Decl -> Maybe Hint
hintDeclPrefixRegexNoCaptures decl@(Block _ "location" [RawString _ op, RawString _ pat] _)
    | op == "~"
    && "^" `isPrefixOf` pat
    && not ("(" `isInfixOf` pat) = Just $
        Hint (ng_getPosition decl) "Loc" "prnc" ("Prefix regex location without captures: " ++ pat ++ " Use: location " ++ tail pat)
    | otherwise = Nothing
hintDeclPrefixRegexNoCaptures _ = Nothing


-- Not used yet.
--hintDeclIfFilename :: Decl -> Maybe Hint
--hintDeclIfFilename decl@(Block _ "if" _ _) = trace (show decl) Nothing
--hintDeclIfFilename _ = Nothing


hintArgExcessiveQuot :: Arg -> Maybe Hint
hintArgExcessiveQuot arg@(QuotedString _ qs) =
    case P.parse plainString "" qs of
         Left _ -> Nothing
         Right (RawString _ s)
            | s == qs -> Just $ Hint (ng_getPosition arg) "Arg" "eq" ("Excessive quoting: \"" ++ qs ++ "\"")
            | otherwise -> Nothing
         -- GHC warns there is non-exhaustive match, missing QuotedString and Integer.
         -- But it seems impossible to get anything other than RawString or parsing error from plainString.
hintArgExcessiveQuot _ = Nothing
