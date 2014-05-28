module TermTests

open AntiUnification
open NUnit.Framework

let check (a: 'a option) (b: 'a option) =
    Assert.AreEqual(a, b, sprintf "%A != %A" a b)

[<TestFixture>]
type ``Anti-unification``() =
    [<Test>]
    member this.``anti-unification of single example is that example``() =
        let x = Function ("f", [Val 1])
        check (Some x) (antiUnify (One x))
    [<Test>]
    // I'd like to use a Theory here but the type of the Datapoints needs to be Term<'a> list list  for a specific 'a_.
    // Can't use Term<System.Object> because an int isn't one of those!
    member this.``anti unification of simple ununifiable examples is logical var``() =
        check (Some (Var "#z0")) (antiUnify (fromList [Val 1; Val 2]))
        check (Some (Var "#z0")) (antiUnify (fromList [Val 1; Val 2; Val 3]))
        check (Some (Var "#z0")) (antiUnify (fromList [Var "x"; Val "y"]))
        check (Some (Var "#z0")) (antiUnify (fromList [Var "#z0"; Val "y"]))
        check (Some (Var "#z0")) (antiUnify (fromList [Var "y"; Val "#z0"]))
        check (Some (Var "#z0")) (antiUnify (fromList [Function ("foo", []); Function ("bar", [])]))
    [<Test>]
    member this.``anti-unification of ununifiable examples is logical var``() =
        check (Some(Var "#z0")) (antiUnify (fromList [Function ("f", [Val 5]); Function ("g", [Val 6])]))
    [<Test>]
    member this.can_generalise_structures_with_logical_vars() =
        check (Some (Var "#z0")) (antiUnify (fromList [Var "#z0"; Var "#z0"]))
        check (Some (Function ("f", [Var "#z0"]))) (antiUnify (fromList [Function ("f", [Val 5]); Function ("f", [Var "var"])]))
        check (Some (Function ("f", [Var "#z0"]))) (antiUnify (fromList [Function ("f", [Var "var"]); Function ("f", [Val 5])]))
    [<Test>]
    member this.``function arity counts``() =
        check (Some(Var "#z0")) (antiUnify (fromList [Function ("f", [Val 5]); Function ("f", [Val 5; Val 5])]))
    [<Test>]
    member this.``function name counts``() =
        check (Some(Var "#z0")) (antiUnify (fromList [Function ("f", [Val 5]); Function ("g", [Val 5])]))
    [<Test>]
    member this.``parts of a structure may become logical vars``() =
        check (Some (Function ("f", [Var "#z0"]))) (antiUnify (fromList [Function ("f", [Val 5]); Function ("f", [Val 6])]))

type BinaryTree<'a> =
    | Node of BinaryTree<'a> * BinaryTree<'a>
    | Leaf of 'a

let rec toTerm = function
    | Node (a, b) -> Function ("Node", [toTerm a; toTerm b])
    | Leaf v -> Val v

[<TestFixture>]
type ``Anti-unification of arbitrary types``() =
    [<Test>]
    member this.``can work (with translation functions)`` () =
        let tree1 = Node (Leaf 1, Leaf 2)
        let tree2 = Node (Leaf 1, Leaf 3)
        let term1 = toTerm tree1
        let term2 = toTerm tree2
        check (Some (Function ("Node",[Function ("Node",[Val 1]); Var "#z0"]))) (antiUnify (fromList [term1; term2]))