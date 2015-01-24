module NginxLint.Parse where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as T

import NginxLint.Data


parseFile :: Parser NgFile
parseFile = do whiteSpace
               pos <- getPosition
               ds <- many decl
               eof
               return $ NgFile (sourceName pos) ds

decl :: Parser Decl
decl = try ifDecl <|> nonIfDecl

nonIfDecl :: Parser Decl
nonIfDecl = try blockDecl <|> oneDecl

oneDecl :: Parser Decl
oneDecl = do whiteSpace
             pos <- getPosition
             name <- identifier
             args <- many argument
             _ <- lexeme (char ';')
             return $ Decl pos name args
          <?> "directive"

blockDecl :: Parser Decl
blockDecl = do whiteSpace
               pos <- getPosition
               name <- identifier
               args <- many argument
               ds <- braces (many decl)
               return $ Block pos name args ds
            <?> "block directive"

ifDecl :: Parser Decl
ifDecl = do whiteSpace
            pos <- getPosition
            reserved "if"
            args <- parens $ many1 ifArgument
            ds <- braces $ many nonIfDecl
            return $ Block pos "if" args ds
         <?> "if directive"
    where
        ifArgument = try parseInteger <|> try quotedString <|> plain
            <?> "if argument"
        plain = mkString " \"\v\t\r\n(){};" "if plain string"

argument :: Parser Arg
argument = try parseInteger <|> try quotedString <|> plainString

parseInteger :: Parser Arg
parseInteger = do pos <- getPosition
                  n <- integer
                  return $ Integer pos n

quotedString :: Parser Arg
quotedString = do pos <- getPosition
                  _ <- symbol q
                  s <- many (noneOf q)
                  _ <- symbol q
                  return $ QuotedString pos s
    where q = "\""

mkString :: String -> String -> Parser Arg
mkString excl help = p <?> help
    where
        p = do
            pos <- getPosition
            s <- lexeme $ many1 (noneOf excl)
            return $ RawString pos s

plainString = mkString " \"\v\t\r\n{};" "plain string"


lexer :: T.TokenParser ()
lexer = T.makeTokenParser nginxDef

nginxDef = emptyDef
    { T.commentLine    = "#"
    , T.identStart     = alphaNum <|> char '_'
    , T.nestedComments = False
    , T.opLetter       = oneOf "<=>"
    , T.reservedNames  = ["if"]
    }

braces        = T.braces lexer
--comma         = T.comma lexer
--commaSep      = T.commaSep lexer
--commaSep1     = T.commaSep1 lexer
--dot           = T.dot lexer
--float         = T.float lexer
identifier    = T.identifier lexer
integer       = T.natural lexer
lexeme        = T.lexeme lexer
--natural       = T.natural lexer
parens        = T.parens lexer
reserved      = T.reserved lexer
--semi          = T.semi lexer
--semiSep       = T.semiSep lexer
symbol        = T.symbol lexer
whiteSpace    = T.whiteSpace lexer
