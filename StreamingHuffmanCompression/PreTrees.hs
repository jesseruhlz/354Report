{-# LANGUAGE DeriveGeneric #-}

module PreTree where

import Control.Applicative          ((<$>), (<*>), (<|>))
import Data.Binary
import Data.List                    (unfoldr)
import Data.Map.Strict              (Map)
import Data.Monoid                  ((<>))
import GHC.Generics
import Weighted
import qualified Data.Map.Strict as M

data PreTree a = PTLeaf a
    | PTNode (PreTree a) (PreTree a)
    deriving (Show, Eq, Generic)

-- put something into empty tree, so create a leaf containing just that data
makePT :: a -> PreTree a
makePT = PTLeaf

-- merge two PreTree a's
mergePT :: PreTree a -> PreTree a -> PreTree a
mergePT = PTNode


type WeightedPT a = Weighted (PreTree a)

makeWPT :: Int -> a -> WeightedPT a
makeWPT w = WPair w . makePT

mergeWPT :: WeightedPT a -> WeightedPT a-> WeightedPT a
mergeWPT (WPair w1 pt1) (WPair w2 pt2)
    = WPair (w1 + w2) (mergePT pt1 pt2)
