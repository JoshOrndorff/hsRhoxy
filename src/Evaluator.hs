module Evaluator
( sub
, autoReduce
) where

import RhoTypes
import qualified Data.Map as Map
import Data.List (delete)

-- A tuplespace is a pair of maps from the channel being communicated
-- over to a list of processes. The first is the sends map
-- and the second is a list of receives
type Tuplespace = (Map.Map Proc [Proc], Map.Map Proc [Proc])
type ProcessPool = [Proc]


sub :: Proc -> String -> Proc -> Proc
sub peg binder target =
  case target of
    Nil g -> Nil g
    Send c p -> Send (sub peg binder c) (sub peg binder p)
    Recv c b p2 -> -- TODO Is this where the `=` operator is important? or shaddowing?
      Recv (sub peg binder c) b (sub peg binder p2)
    FreeName f -> if binder == f then peg else FreeName f
    Par ps -> Par $ map (sub peg binder) $ ps


growTuplespace :: Proc -> Tuplespace -> Tuplespace
growTuplespace s@(Send c _)   (sends, recvs) = (Map.insertWith (++) c [s] sends, recvs)
growTuplespace r@(Recv c _ _) (sends, recvs) = (sends, Map.insertWith (++) c [r] recvs)

pruneTuplespace :: Proc -> Tuplespace -> Tuplespace
pruneTuplespace s@(Send c _)   (sends, recvs) = (Map.adjust (delete s) c sends, recvs)
pruneTuplespace r@(Recv c _ _) (sends, recvs) = (sends, Map.adjust (delete r) c recvs)
--TODO When removing the last send (or recv) on a particular channel,
-- I should probably remove the key from the map entirely. Maybe update instead of adjust?

-- autoReduce takes in a tuplespace and and reduces it to a
-- quiescent tuplespace non-interactively. No promises are
-- made about how nondeterminism is resolved.
-- Error messages are returned as strings

autoReduce :: ProcessPool -> Either String Tuplespace
autoReduce = autoReduce' (Map.empty, Map.empty)

autoReduce' :: Tuplespace -> ProcessPool -> Either String Tuplespace
autoReduce' t [] = Right t
autoReduce' t@(sends, recvs) (p:ool) =
  case p of
    (Nil _) -> autoReduce' t ool
    (Par newPool) -> autoReduce' t (newPool ++ ool) -- Will this ever be relevant
    (FreeName n) -> Left $ "Name " ++ n ++ " was not bound"
        -- TODO Scan AST for freenames before evaluating
    s@(Send c m) ->
      case Map.lookup c recvs of
        Just (r@(Recv _ b cont):_) -> autoReduce' (pruneTuplespace r t) $ (sub m b cont):ool
        _                          -> autoReduce' (growTuplespace s t) ool
    r@(Recv c b cont) ->
      case Map.lookup c sends of
        Just (s@(Send _ m):_) -> autoReduce' (pruneTuplespace s t) $ (sub m b cont):ool
        _                          -> autoReduce' (growTuplespace r t) ool

-- Autoreduce is a simple and efficient way to view one possible reduction
-- of a program. It uses a structured tuplespace and greedily exicutes comms.
-- Another option is to process the entire incoming process pool into the
-- tuplespace and only then (possibly interactively or algorithmiaclly) choose
-- which comm even to reduce next. This second option allows for debugging
-- observation of execution, and forcing specific nondeterministic outcomes.


-- Returns a list of all free names in a process. Useful to ensure
-- there are no top-level free names.
-- My instinct was that we wouldn't be comming over processes
-- with freenames in them either, but now I can't think of a
-- good reason not to.
freenames :: Proc -> [String]
freenames (Nil _) = []
freenames (Par ps) = foldr (++) [] $ map freenames ps -- is there a better-style way to flatten?
freenames (FreeName n) = [n]
freenames (Send c m) = (freenames c) ++ (freenames m)
freenames (Recv c binder cont) = (freenames c) ++ (delete binder $ freenames cont)
