module Data.PLTree where

import Data.PLprop

-- |Binary branching trees
data Tree a = Leaf [TProp] | DeadLeaf [TProp] | Branch [TProp] (Tree [TProp], Tree [TProp])
    deriving (Eq,Show)

-- |Trees are built from "tagged" propositions
type TProp = (Prop,Bool)