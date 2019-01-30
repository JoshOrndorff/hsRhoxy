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
  name <- many1 letter
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
parseRhoc :: String -> Either ParseError Par
parseRhoc = parse parsePar "useless-string"
