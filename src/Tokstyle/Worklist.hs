module Tokstyle.Worklist (
    Worklist,
    empty,
    fromList,
    push,
    pushList,
    pop,
    toList
) where

import qualified Data.Foldable as F
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

newtype Worklist a = Worklist (Seq a)
    deriving (Show, Eq)

empty :: Worklist a
empty = Worklist Seq.empty

fromList :: [a] -> Worklist a
fromList = Worklist . Seq.fromList

push :: a -> Worklist a -> Worklist a
push a (Worklist s) = Worklist (s |> a)

pushList :: [a] -> Worklist a -> Worklist a
pushList l (Worklist s) = Worklist (s <> Seq.fromList l)

pop :: Worklist a -> Maybe (a, Worklist a)
pop (Worklist s) =
    case Seq.viewl s of
        Seq.EmptyL  -> Nothing
        a Seq.:< s' -> Just (a, Worklist s')

toList :: Worklist a -> [a]
toList (Worklist s) = F.toList s
