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
    filler
    p <- parseProc
    filler
    return p
  filler
  return $ Par ps

parseNil :: Parser Proc
parseNil = Nil <$ string "Nil" -- This is _not_ applicitave [functor], it is just functor

-- parseUnquote :: Parser Proc
-- parseUnquote = do
--   _    <- char '*'
--   chan <- parseChan
--   return $ Unquote chan

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
  subPattern <- between (char '(') (char ')') parseFreeName
  continuation <- between (char '{') (char '}') parseProc
  return $ Recv chan subPattern continuation

-- for (arg <- chan) { Proc }
-- parseRecv = do
--   _ <- string "for ("
--   subpattern <- parseFreeName
--   _ <- string "<-"
--   chan <- parseChan
--   _ <- ')'
--   continuation <- parseProc
--   return $ Recv $ chan subpattern continuation


parseFreeName :: Parser Proc
parseFreeName = do
  name <- many1 letter
  return $ FreeName name

parseProc :: Parser Proc
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
