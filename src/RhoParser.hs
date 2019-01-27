module RhoParser
(parseRhoc
, Proc (..)
, Chan (..)
, Par (..)
) where

-- TODO Parse rholang-style comments
-- TODO Make Par a larger thing


import Text.Parsec
import Text.Parsec.String


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
  _  <- optional$ char '|'
  p <- parseProc -- A Par must contain at least one Proc
  ps <- many $ do
            _ <- char '|'
            parseProc -- I think I can leave it naturally in the monad, right?
  _  <- optional$ char '|'
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
     -- <|> try parsePar
        <|> try parseUnquote
        <|> parseHole

parseChan :: Parser Chan
parseChan = do
  _ <- char '@'
  p <- parseProc
  return $ Quote p

-- Exported for the client
parseRhoc :: String -> Either ParseError Par
parseRhoc = parse parsePar "useless-string"
