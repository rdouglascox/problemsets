module Data.GPLITree where

import Data.GPLIprop

-- |Data type for Proof Trees
data PTree = Leaf [AProp] | DeadLeaf [AProp] | Branch [AProp] (PTree, PTree)
    deriving (Eq,Show)

-- |Data type for annotated propositions. 
data AProp = AProp Prop Check Subs
    deriving (Eq,Show)

-- |Type synonyms for Checkmarks and Substitutions
type Check = Bool
type Subs = [Char]


