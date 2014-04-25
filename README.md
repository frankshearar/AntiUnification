AntiUnification
===============

A functional implementation of the anti-unification algorithm for multiple terms.

Note that this is the first F# project of any consequence that I've written. If you think the code's awful, please let me know!

So... what's that mean?
-----------------------

Suppose we have two structures - trees A and B, perhaps. Each structure has holes in it. A has a left subtree with a leaf value of 1 and a right subtree with leaf value "x" - a placeholder, or variable.. B has a left subtree with leaf value "y" - another placeholder - and a right subtree with leaf value 2. What values would x and y need to have for A and B to be equal? We call the process of discovering this mapping "unification".

Anti-unification, as its name suggests, is the inverse operation. Given a pair of terms, differing part, what parameterised prototypical structure could - with suitable substitutions - yield the original structures? That is, anti-unification yields the _least general generalisetion_ of the set of structures.

Suppose we have the data type

````F#
type Term<'a> =
    | Function of Symbol * Term<'a> list
    | Val of 'a
    | Var of string
````

Then

````F#
// The examples differ completely - the best you can say is "the general example is something":
antiUnify [Val 1; Val 2] = Some (Var "#z0")
// The examples differ only in the argument to the function, so we parameterise only that:
antiUnify [Function ("f", [Val 5]); Function ("f", [Val 6])] = Some (Function ("f", [Var "#z0"]))

Further Reading
-------------

Bjarte M. Østvold's paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.95.9108&rep=rep1&type=pdf
William Byrd's Scheme implementation: https://github.com/webyrd/anti-unification