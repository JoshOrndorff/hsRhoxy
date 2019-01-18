-- Simple monadic rho-calc interpreter
module Main where

-- For getting commandline arguments
import System.Environment

-- For parsing
import Text.Parsec
import Text.Parsec.String

-- For executing
--import...

-- All productions in the rho calc
data Proc = Nil
          | Unquote Chan
          | Send Chan Proc        -- Comm channel, Process being sent
          | Recv Chan String Proc -- Comm channel, Name being bound, continuation
          | Par Proc Proc         -- Also consider Par [Proc]
          | Hole String           -- Used for substituting
          deriving Show

data Chan = Quote Proc
          deriving Show


parseNil :: Parser Proc
parseNil = Nil <$ string "Nil" -- This is _not_ applicitave [functor], it is just functor

parseUnquote :: Parser Proc
parseUnquote = do
  _        <- char '*'
  Quote(p) <- parseChan
  return p

parseSend :: Parser Proc
parseSend = do
  c <- parseChan
  _ <- char '!'
  p <- between (char '(') (char ')') parseProc
  return $ Send c p

parseRecv :: Parser Proc
parseRecv = undefined

parsePar :: Parser Proc
parsePar = undefined

parseHole :: Parser Proc
parseHole = do
  name <- many letter
  return $ Hole name

parseProc :: Parser Proc
parseProc = try parseNil
        <|> try parseSend
     -- <|> try parseRecv
     -- <|> try parsePar
        <|> try parseUnquote
        <|> parseHole

parseChan :: Parser Chan
parseChan = do
  _ <- char '@'
  p <- parseProc
  return $ Quote p



main :: IO ()
main = do
  args <- getArgs
  prog <- readFile $ head args
  print $ parse parseProc "useless-string" prog
