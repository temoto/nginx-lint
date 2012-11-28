{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
module NginxLint.Data where

import Data.Data (Data, Typeable)
import Data.Generics.Str (listStr, strList, Str(Zero))
import Data.Generics.Uniplate.Operations (Biplate, biplate, Uniplate, uniplate)
import qualified Text.ParserCombinators.Parsec as P


type Ident = String

data NgFile = NgFile FilePath [Decl]
    deriving (Eq, Show)

data Arg
    = RawString P.SourcePos String
    | QuotedString P.SourcePos String
    | Integer P.SourcePos Integer
    deriving (Data, Eq, Show, Typeable)

data Decl
    = Decl P.SourcePos Ident [Arg]
    | Block P.SourcePos Ident [Arg] [Decl]
    | If P.SourcePos [Arg] [Decl]
    | Location P.SourcePos [Arg] [Decl]
    deriving (Data, Eq, Show, Typeable)

instance Biplate NgFile Decl where
    biplate (NgFile fname ds) = (listStr ds, \newds -> NgFile fname (strList newds))

instance Uniplate Decl where
    uniplate d@Decl{} = (Zero, \Zero -> d)
    uniplate (Block pos ident args children) = (listStr children, \newds -> Block pos ident args (strList newds))
    uniplate (If    pos       args children) = (listStr children, \newds -> If pos args (strList newds))
    uniplate (Location pos    args children) = (listStr children, \newds -> If pos args (strList newds))


class NgPositioned a where
    ng_getPosition :: a -> P.SourcePos


instance NgPositioned Arg where
    ng_getPosition (RawString pos _) = pos
    ng_getPosition (QuotedString pos _) = pos
    ng_getPosition (Integer pos _) = pos

instance NgPositioned Decl where
    ng_getPosition (Decl pos _ _) = pos
    ng_getPosition (Block pos _ _ _) = pos
    ng_getPosition (If pos _ _) = pos
    ng_getPosition (Location pos _ _) = pos


-- Hint where, category, id, content
data Hint = Hint P.SourcePos String String String


instance NgPositioned Hint where
    ng_getPosition (Hint pos _ _ _) = pos


-- pretty print for P.SourcePos
ppSrcPos :: P.SourcePos -> String
ppSrcPos pos = P.sourceName pos ++ ":" ++ show (P.sourceLine pos)
               ++ ":" ++ show (P.sourceColumn pos)

-- pretty print for Arg
ppArg :: Arg -> String
ppArg (RawString    _ s) = s
ppArg (QuotedString _ s) = "\"" ++ s ++ "\""
ppArg (Integer      _ i) = show i

ppArgList :: [Arg] -> String
ppArgList = concatMap (\a -> " " ++ ppArg a)

-- pretty print for Decl
ppDecl :: Decl -> String
ppDecl (Decl  _ name args   ) = name ++ ppArgList args ++ ";\n"
ppDecl (Block _ name args ds) = name ++ ppArgList args ++ " {\n"
                                ++ concatMap (\d -> "    " ++ ppDecl d) ds
                                ++ "}\n"
ppDecl (If _ args ds) = "if ( " ++ ppArgList args ++ " ) {\n"
                          ++ concatMap (\d -> "    " ++ ppDecl d) ds
                          ++ "}\n"
ppDecl (Location pos args ds) = ppDecl $ Block pos "location" args ds


-- pretty print for Hint
ppHint :: Hint -> String
ppHint h@(Hint _ cat ident content) = ppSrcPos pos ++ " "
        ++ cat ++ ":" ++ ident ++ ": " ++ content
    where pos = ng_getPosition h
