module RhoTypes where

data Proc = Par [Proc]
          | Send {channel :: Proc, message :: Proc}
          | Recv {channel :: Proc, subPattern :: Proc, continuation :: Proc}
          | FreeName String
          | Nil --TODO (Ground a => Maybe a)
          deriving (Show, Eq)

-- data Primative = RhoString
--                | RhoInt
--
-- class Ground a where
--   rhoType :: a -> Primative
--
-- instance Ground Int where
--   rhoType _ = RhoInt
--
-- instance Ground String where
--   rhoType _ = RhoString
