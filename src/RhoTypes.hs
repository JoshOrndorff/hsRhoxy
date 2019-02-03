module RhoTypes
( Proc
, nil
, Proc' (Par, Send, Recv, FreeName, Nil)
) where


-- We're only doing basic rho calc, so don't make
-- client's deal with generality of ground terms yet
type Proc = Proc' ()
nil = Nil ()

-- Processes are parametric in a ground type.
-- For basic rho calc with a single ground, () will do
-- For rholang, use a datatype like Primative below

data Proc' a =
    Par [Proc' a] -- TODO use this tag OR type Par = [Proc]
  | Send {channel :: Proc' a, message :: Proc' a}
  | Recv {channel :: Proc' a, subPattern :: String, continuation' :: Proc' a}
  | FreeName String
  | Nil a
  deriving (Show, Ord, Eq)
-- it's a very strong convention in Haskell to never add typeclass constraints in data declarations


data Primative = RhoString
               | RhoInt
