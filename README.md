AntiUnification
===============

A functional implementation of the anti-unification algorithm for multiple terms.

Note that this is the first F# project of any consequence that I've written. If you think the code's awful, please let me know!

So... what's that mean?
-----------------------

Suppose we have two structures - trees A and B, perhaps. Each structure has holes in it. A has a left subtree with a leaf value of 1 and a right subtree with leaf value "x" - a placeholder, or variable.. B has a left subtree with leaf value "y" - another placeholder - and a right subtree with leaf value 2. What values would x and y need to have for A and B to be equal? We call the process of discovering this mapping "unification".

Anti-unification, as its name suggests, is the inverse operation. Given a pair of terms, differing part, what parameterised prototypical structure could - with suitable substitutions - yield the original structures? That is, anti-unification yields the _least general generalisetion_ of the set of structures.

Suppose we have the data type

````fsharp
type Term<'a> =
    | Function of Symbol * Term<'a> list
    | Val of 'a
    | Var of string
    | Const of string
````

then

````fsharp
// The examples differ only in the argument to the function, so we parameterise only that:
antiUnify (Many (Function ("f", [Val 5]), One (Function ("f", [Val 6])))) = Function ("f", [Var "#z0"])

// The examples differ completely - the best you can say is "the general example is something":
antiUnify (Many (Val 1, One (Val 2))) = Var "#z0"
````

To anti-unify an arbitrary data structure, you will need to write translation routines to and from `Term<'a>`. For instance,

````fsharp
type BinaryTree<'a> =
    | Node of BinaryTree<'a> * BinaryTree<'a>
    | Leaf of 'a

let rec toTerm = function
    | Node (a, b) -> Function ("Node", [toTerm a; toTerm b])
    | Leaf v -> Val v
````

The module supplies `fromList`, which translates from a `list` to a `TermSequence<'a>`. This lets you easily use anti-unification, while preserving the algorithm's requirement to have at least one example.

[![Build status](https://ci.appveyor.com/api/projects/status/aowh0espi50oqd1l)](https://ci.appveyor.com/project/frankshearar/antiunification)

Further Reading
---------------

Bjarte M. Østvold's paper            | http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.95.9108&rep=rep1&type=pdf
-------------------------------------|-----------------------------------------------------------------------------------
William Byrd's Scheme implementation | https://github.com/webyrd/anti-unification