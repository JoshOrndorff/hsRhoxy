module RhoTypes where

data Proc = Par [Proc] -- TODO use this tag OR type Par = [Proc]
          | Send {channel :: Proc, message :: Proc}
          | Recv {channel :: Proc, subPattern :: Proc, continuation :: Proc}
          | FreeName String
          | Nil --TODO (Ground a => Maybe a)
    --  it's a very strong convention in Haskell to never add typeclass constraints in data declarations.
          deriving (Show, Ord, Eq)

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
