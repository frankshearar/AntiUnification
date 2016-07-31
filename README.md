AntiUnification
===============

A functional implementation of the anti-unification algorithm for multiple terms.

Note that this is the first F# project of any consequence that I've written. If you think the code's awful, please let me know!

So... what's that mean?
-----------------------

Suppose we have two structures - trees A and B, perhaps. Each structure has holes in it. A has a left subtree with a leaf value of 1 and a right subtree with leaf value "x" - a placeholder, or variable.. B has a left subtree with leaf value "y" - another placeholder - and a right subtree with leaf value 2. What values would x and y need to have for A and B to be equal? We call the process of discovering this mapping "unification".

Anti-unification, as its name suggests, is the inverse operation. Given a pair of terms, differing in part, what parameterised prototypical structure could - with suitable substitutions - yield the original structures? That is, anti-unification yields the _least general generalisetion_ of the set of structures.

Suppose we have the data type

````fsharp
type Term<'a> =
    | Function of Symbol * Term<'a> list
    | Val of 'a
    | Var of string
    | Const of string
````

then, given that `fromList` translates from a `list` to a `TermSequence<'a>`, we have that

````fsharp
// The examples differ only in the argument to the function, so we parameterise only that:
antiUnify (fromList [Function ("f", [Val 5]); Function ("f", [Val 6])]) = Function ("f", [Var "#z0"])

// Here the examples differ completely - the best you can say is "the general example is something":
antiUnify (fromList [Val 1; Val 2]) = Var "#z0"
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

[![Build status on .NET](https://ci.appveyor.com/api/projects/status/aowh0espi50oqd1l)](https://ci.appveyor.com/project/frankshearar/antiunification)
[![Build Status on Mono](https://secure.travis-ci.org/frankshearar/AntiUnification.png?branch=master)](http://travis-ci.org/frankshearar/AntiUnification)

Further Reading
---------------

Reference                            | URL
-------------------------------------|-----------------------------------------------------------------------------------
Bjarte M. Østvold's paper            | http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.95.9108&rep=rep1&type=pdf
William Byrd's Scheme implementation | https://github.com/webyrd/anti-unification
Applications                         | https://en.wikipedia.org/wiki/Anti-unification_(computer_science)#Applications

Licence
-------

Copyright (C) 2013-2014 by Frank Shearar

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.