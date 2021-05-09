---
title: Proof Trees in Haskell
author: Ryan Cox
---

Let's begin with an informal description of an algorithm for building a proof
tree for general predicate logic with identity (GPLI). 

1.

We begin by setting up the root node of the tree. The root node will consist of
a non-empty list of propositions. For example:

     (Fa->Gb)
    @x(Fx->Gx)
       #xGa

Technically, this is just a list, and we could represent it like this:
 
     [(Fa->Gb), @x(Fx->Gx), #xGa]

Indeed, it is not just a list, but it is a non-branching node of a tree.

2.

Okay, so the next thing we want to do is check to see whether there are any
contradictions on the single path of our newly created tree. This will just be
an application of a more general function for testing for contradictions
applied in the special case of a leaf node.

3.

The next thing we want to do is determine which rule we should apply next, if,
indeed, we are apply to apply any rule at all. It is generally a good idea to
apply PL non-branching rules first, followed by PL branching rules, then
existential, then universal, then SI, if we have a choice. We may sometimes
have a choice of PL non-branching rules, or, indeed, a choice of any rule. As
humans, constructing the tree on paper, we can directly target the relevant
proposition to apply the rule to, applying the rule to the open paths the
proposition sits on. To keep things simple, we will just apply a particular
rule to the first unchecked proposition of the relevant kind that we find as we
move down the tree.




We have to make a decision about the data structure for trees. We might
naturally take the trees to be binary trees thus. 

```haskell
Data Tree a = Branch a (Tree a, Tree a) | Leaf Bool a

```

Why the extra argument for leafs? Well, we want to know whether a path on a
tree is closed or not. We could use False to indicate a closed path and Tree to
indicate an open path.


