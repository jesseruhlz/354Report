{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}

module PQueue
(
    PQueue
    , emptyPQ
    , insertPQ
    , popPQ
    , sizePQ
    , SkewHeap(..), makeSH, popSH, mergeSH
)where

import Data.Foldable

data SkewHeap a = SEmpty
                | SNode a (SkewHeap a) (SkewHeap a)
                deriving (Show, Eq, Foldable)

makeSH :: a -> SkewHeap a
makeSH x = SNode x SEmpty SEmpty

popSH :: Ord a => SkewHeap a-> (Maybe a, SkewHeap a)
popSH SEmpty = (Nothing, SEmpty)
popSH (SNode r h1 h2) = (Just r, mergeSH h1 h2)

mergeSH :: Ord a => SkewHeap a-> SkewHeap a -> SkewHeap a
mergeSH SEmpty h = h
mergeSH h SEmpty = h
mergeSH hA@(SNode xA lA rA) hB@(SNode xB lB rB)
    | xA < xB = SNode xA (mergeSH rA hB) lA
    | otherwise = SNode xB (mergeSH rB hA) lB

newtype PQueue a = PQ (SkewHeap a) deriving Show

emptyPQ :: PQueue a
emptyPQ = PQ SEmpty

insertPQ :: Ord a => a -> PQueue a -> PQueue a
insertPQ x (PQ h) = PQ (mergeSH h (makeSH x))

popPQ :: Ord a => PQueue a -> (Maybe a, PQueue a)
popPQ (PQ h) = (res, PQ h')
    where 
        (res, h') = popSH h

sizePQ :: PQueue a -> Int
sizePQ (PQ h) = length (toList h)