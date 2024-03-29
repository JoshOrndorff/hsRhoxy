module RhoParser
(parseRhoc
) where


import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char (spaces)

import RhoTypes

parsePar :: Parser Proc
parsePar = do
  filler
  optional $ char '|'
  ps <- (flip sepEndBy1) (char '|') $ do
    -- TODO How to tell parsec that _anything_ can be surrounded by filler?
    -- You can't
    filler
    p <- parseProc
    filler
    return p
  filler
  return $ Par ps

parseNil :: Parser Proc
parseNil = nil <$ string "Nil"

parseSend :: Parser Proc
parseSend = do
  c <- parseChan
  _ <- char '!'
  p <- between (char '(') (char ')') parseProc
  return $ Send c p -- TODO How does this actually typecheck? Don't I need a record type?

parseRecv :: Parser Proc
-- @Nil?(x){P}
parseRecv = do
  chan <- parseChan
  char '?'
  binder <- between (char '(') (char ')') parseBinder
  continuation <- between (char '{') (char '}') parseProc
  return $ Recv chan binder continuation

-- for (arg <- chan) { Proc }
-- parseRecv = do
--   _ <- string "for ("
--   subpattern <- parseFreeName
--   _ <- string "<-"
--   chan <- parseChan
--   _ <- ')'
--   continuation <- parseProc
--   return $ Recv $ chan subpattern continuation

parseBinder = many1 letter

parseFreeName :: Parser Proc
parseFreeName = do
  name <- parseBinder
  return $ FreeName name

parseProc :: Parser Proc
--TODO maybe move filler stuff here.
parseProc = try parseNil
        <|> try parseSend
        <|> try parseRecv
        <|> parseFreeName

parseChan :: Parser Proc
parseChan = do
  char '@'
  parseProc

lineComment :: Parser ()
lineComment = do
  _ <- string "//"
  skipMany (satisfy (/= '\n'))

blockComment :: Parser ()
blockComment = do
  _ <- string "/*"
  _ <- manyTill anyChar (try (string "*/"))
  return ()


filler :: Parser ()
filler = skipMany $
  (try (skipMany1 space) <|> try lineComment <|> try blockComment)


-- Exported for the client
--TODO How to make sure I've parsed the entire file?
parseRhoc :: String -> Either ParseError Proc
parseRhoc = parse parsePar "useless-string"
