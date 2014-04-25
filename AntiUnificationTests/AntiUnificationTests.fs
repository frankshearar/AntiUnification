module TermTests

open AntiUnification
open NUnit.Framework

let check (a: 'a option) (b: 'a option) =
    Assert.AreEqual(a, b, sprintf "%A != %A" a b)

[<TestFixture>]
type anti_unification() =
    [<Test>]
    member this.must_have_at_least_one_example() =
        check None (antiUnify [])
    [<Test>]
    member this.anti_unification_of_single_example_is_that_example() =
        let x = Function ("f", [Val 1])
        check (Some x) (antiUnify [x])
    [<Test>]
    // I'd like to use a Theory here but the type of the Datapoints needs to be Term<'a> list list _for a specific 'a_.
    // Can't use Term<System.Object> because an int isn't one of those!
    member this.anti_unification_of_simple_ununifiable_examples_is_logical_var() =
        check (Some (Var "#z0")) (antiUnify [Val 1; Val 2])
        check (Some (Var "#z0")) (antiUnify [Val 1; Val 2; Val 3])
        check (Some (Var "#z0")) (antiUnify [Var "x"; Val "y"])
        check (Some (Var "#z0")) (antiUnify [Var "#z0"; Val "y"])
        check (Some (Var "#z0")) (antiUnify [Var "y"; Val "#z0"])
        check (Some (Var "#z0")) (antiUnify [Function ("foo", []); Function ("bar", [])])
    [<Test>]
    member this.anti_unification_of_ununifiable_examples_is_logical_var() =
        check (Some(Var "#z0")) (antiUnify [Function ("f", [Val 5]); Function ("g", [Val 6])])
    [<Test>]
    member this.can_generalise_structures_with_logical_vars() =
        check (Some (Var "#z0")) (antiUnify [Var "#z0"; Var "#z0"])
        check (Some (Function ("f", [Var "#z0"]))) (antiUnify [Function ("f", [Val 5]); Function ("f", [Var "var"])])
        check (Some (Function ("f", [Var "#z0"]))) (antiUnify [Function ("f", [Var "var"]); Function ("f", [Val 5])])
    [<Test>]
    member this.function_arity_counts() =
        check (Some(Var "#z0")) (antiUnify [Function ("f", [Val 5]); Function ("f", [Val 5; Val 5])])
    [<Test>]
    member this.function_name_counts() =
        check (Some(Var "#z0")) (antiUnify [Function ("f", [Val 5]); Function ("g", [Val 5])])
    [<Test>]
    member this.parts_of_a_structure_may_become_logical_vars() =
        check (Some (Function ("f", [Var "#z0"]))) (antiUnify [Function ("f", [Val 5]); Function ("f", [Val 6])])