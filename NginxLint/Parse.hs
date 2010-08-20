module NginxLint.Parse where

import Debug.Trace
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

nonIfDecl = try blockDecl <|> oneDecl

oneDecl = do whiteSpace
             pos <- getPosition
             name <- identifier
             args <- many argument
             lexeme (char ';')
             return $ Decl pos name args
          <?> "directive"

blockDecl = do whiteSpace
               pos <- getPosition
               name <- identifier
               args <- try (many argument)
               ds <- braces (many decl)
               return $ Block pos name args ds

ifDecl = do whiteSpace
            pos <- getPosition
            reserved "if"
            symbol "("
            args <- argument `manyTill` try (symbol ")")
            ds <- braces (many nonIfDecl)
            return $ Block pos "if" args ds

argument :: Parser Arg
argument = quotedString <|> plainString
        <|> parseInteger
        <?> "directive argument"

parseInteger = do pos <- getPosition
                  n <- integer
                  return $ Integer pos n

quotedString = do pos <- getPosition
                  symbol "\""
                  s <- many (noneOf "\"")
                  symbol "\""
                  return $ QuotedString pos s

plainString = do pos <- getPosition
                 s <- lexeme ps
                 return $ RawString pos s
              <?> "plain string"
    where ps = many1 (noneOf " \"\v\t\r\n(){};")


lexer :: T.TokenParser ()
lexer = T.makeTokenParser nginxDef

nginxDef = emptyDef
    { T.commentLine    = "#"
    , T.nestedComments = False
    , T.opLetter       = oneOf "<=>"
    , T.reservedNames  = ["if"]
    }

whiteSpace    = T.whiteSpace lexer
lexeme        = T.lexeme lexer
symbol        = T.symbol lexer
braces        = T.braces lexer
natural       = T.natural lexer
float         = T.float lexer
integer       = T.natural lexer
parens        = T.parens lexer
comma         = T.comma lexer
semi          = T.semi lexer
dot           = T.dot lexer
identifier    = T.identifier lexer
reserved      = T.reserved lexer
commaSep      = T.commaSep lexer
commaSep1     = T.commaSep1 lexer
semiSep       = T.semiSep lexer
