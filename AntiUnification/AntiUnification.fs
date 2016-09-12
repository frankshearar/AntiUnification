module AntiUnification

type Symbol = string

type Term<'a> =
    | Function of Symbol * Term<'a> list
    | Val      of 'a
    | Var      of string
    | Const    of string

type TermSequence<'a> =
    | One  of 'a
    | Many of 'a * TermSequence<'a>

let rec fold f seed = function
    | One x        -> f seed x
    | Many (x, xs) -> fold f x xs

let rec fromList = function
    | []    -> failwith "Cannot convert [] to a NonEmptyList"
    | [t]   -> One t
    | t::ts -> Many (t, fromList ts)

let head = function
    | One x        -> x
    | Many (x, xs) -> x

let rec map f = function
    | One x        -> One (f x)
    | Many (x, xs) -> Many (f x, map f xs)

let foreach xs f = map f xs

// forall returns true iff a predicate is true for every element in a non-empty list.
let forall f xs = fold (&&) true (map f xs)

// Given a TermSequence of Functions, heads returns a TermSequence containing the heads of the argument lists.
let heads xs = foreach xs (function
    | Function (name, t::ts) -> t
    | unrecognised           -> failwith (sprintf "heads can only process a Function, not %A" unrecognised))

// Given a TermSequence of Functions, tails returns a TermSequence containing Functions with the tails of the argument lists.
let tails xs = foreach xs (function
    | Function (name, t::ts) -> Function (name, ts)
    | unrecognised           -> failwith (sprintf "tails can only process a Function, not %A" unrecognised))

// hasMap returns true iff the substitution maps a term sequence.
let hasMap key substitution =
    List.exists ((=) key << fst) substitution

// findMap returns the map for a term sequence in a substitution.
let findMap key substitution =
    List.find ((=) key << fst) substitution

let var base_name n = Var (sprintf "%s%d" base_name n)

// allEqual returns true if all elements in the sequence are equal.
let allEqual = function
    | One _        -> true
    | Many (x, xs) -> forall ((=) x) xs

// functionUnifiable returns true if the given Function has the expected name and arity.
let functionUnifiable name1 arity = function
    | Function (name2, args2) -> name2 = name1 && args2.Length = arity
    | _                       -> false

// allFunctionUnifiable returns true iff all elements in the sequence are Functions with the same name and the same arity.
let allFunctionUnifiable = function
    | Many (Function (f, args), tail) -> forall (functionUnifiable f args.Length) tail
    | _                               -> false

// antiUnifyTheta is a helper for antiUnify, returning the anti-unification of a term sequence with
// respect to a substitution map (mapping term sequences to logical variables)
// n contains the number of logical variables found so far: it lets us create fresh logical variables.
// TODO: At the moment, we use logical variables of the form Var "#z0", Var "#z1", Var "#z2", ... This
// will cause problems is a user happens to also use names in this namespace.
let rec antiUnifyTheta (theta: (TermSequence<Term<'a>> * Term<'a>) list) n = function

    // rule 7: all examples the same? return the first
    | One x                                       -> (x, theta, n)
    | examples when allEqual examples             -> (head examples, theta, n)

    // rule 8: recurse into the Function arguments
    | examples when allFunctionUnifiable examples -> let (arg, theta', n') = antiUnifyTheta theta n (heads examples)
                                                     match antiUnifyTheta theta' n' (tails examples) with
                                                        | (Function (f, args), theta'', n'') -> (Function (f, arg::args), theta'', n'')
                                                        | unrecognised                       -> failwith (sprintf "Found %A rather than Function" unrecognised)

    // rule 9: return the previously mapped logical variable
    | examples when hasMap examples theta         -> (snd (findMap examples theta), theta, n)

    // rule 10: introduce a "fresh" logical variable (see TODO)
    | examples                                    -> let z = var "#z" n
                                                     (z, ((examples, z) :: theta), n + 1)

// preprocess turns all Vars into ground terms by pretending they're constant terms.
let rec preprocess = function
    | Function (name, args) -> Function (name, List.map preprocess args)
    | Var name              -> Const name
    | x                     -> x

// postprocess is the inverse of preprocess: it turns "ground" Consts back into ungrounded variables.
let rec postprocess = function
    | Function (name, args) -> Function (name, List.map postprocess args)
    | Const name            -> Var name
    | x                     -> x

// antiUnify returns the least general generalisation of a list of example terms (a term sequence).
let antiUnify examples =
    let processed_examples          = map preprocess examples
    let (generalised_example, _, _) = antiUnifyTheta [] 0 processed_examples
    postprocess generalised_example