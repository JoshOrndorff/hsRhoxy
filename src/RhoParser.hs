module RhoParser
(parseRhoc
, Proc (..)
, Chan (..)
, Par (..)
) where


import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char (spaces)


data Proc = Nil
          | Unquote Chan
          | Send Chan Proc        -- Comm channel, Process being sent
          | Recv Chan Proc Proc   -- Comm channel, Name being bound, Continuation
          | Hole String           -- Used for substituting
          deriving (Show, Eq)

data Chan = Quote Proc
          deriving (Show, Eq)

data Par = Par [Proc]
          deriving (Show, Eq)

parsePar :: Parser Par
parsePar = do
  _ <- many parseFiller
  _ <- optional$ char '|'
  _ <- many parseFiller
  p <- parseProc -- A Par must contain at least one Proc
  ps <- many $ do
        _ <- many parseFiller
        _ <- char '|'
        _ <- many parseFiller
        parseProc
  _ <- many parseFiller
  _ <- optional$ char '|'
  return $ Par (p:ps)

parseNil :: Parser Proc
parseNil = Nil <$ string "Nil" -- This is _not_ applicitave [functor], it is just functor

parseUnquote :: Parser Proc
parseUnquote = do
  _    <- char '*'
  chan <- parseChan
  return $ Unquote chan

parseSend :: Parser Proc
parseSend = do
  c <- parseChan
  _ <- char '!'
  p <- between (char '(') (char ')') parseProc
  return $ Send c p

parseRecv :: Parser Proc
-- @Nil?(x){P}
parseRecv = do
  chan <- parseChan
  _ <- char '?'
  subpattern <- between (char '(') (char ')') parseHole
  continuation <- between (char '{') (char '}') parseProc
  return $ Recv chan subpattern continuation

-- for (arg <- chan) { Proc }
-- parseRecv = do
--   _ <- string "for ("
--   subpattern <- parseHole
--   _ <- string "<-"
--   chan <- parseChan
--   _ <- ')'
--   continuation <- parseProc
--   return $ Recv $ chan subpattern continuation


parseHole :: Parser Proc
parseHole = do
  name <- many letter
  return $ Hole name

parseProc :: Parser Proc
parseProc = try parseNil
        <|> try parseSend
        <|> try parseRecv
        <|> try parseUnquote
        <|> parseHole

parseChan :: Parser Chan
parseChan = do
  _ <- char '@'
  p <- parseProc
  return $ Quote p

parseLineComment :: Parser ()
parseLineComment = do
  _ <- string "//"
  _ <- manyTill anyChar endOfLine
  return ()

parseBlockComment :: Parser ()
parseBlockComment = do
  _ <- string "/*"
  _ <- manyTill anyChar (try (string "*/"))
  return ()

--TODO It could be several comments with space in between
parseFiller :: Parser ()
parseFiller = do
  _ <- spaces
  _ <- try parseLineComment <|> try parseBlockComment
  spaces


-- Exported for the client
parseRhoc :: String -> Either ParseError Par
parseRhoc = parse parsePar "useless-string"
