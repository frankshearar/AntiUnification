module AntiUnification

type Symbol = string

type Term<'a> =
    | Function of Symbol * Term<'a> list
    | Val of 'a
    | Var of string
    | Const of string

type TermSequence<'a> =
    | One of 'a
    | Many of 'a * TermSequence<'a>

let rec fold f seed = function
    | One x -> f seed x
    | Many (x, xs) -> fold f x xs

let rec fromList = function
    | [] -> failwith "Cannot convert [] to a NonEmptyList"
    | [t] -> One t
    | t::ts -> Many (t, fromList ts)

let head = function
    | One x -> x
    | Many (x, xs) -> x

let rec map f = function
    | One x -> One (f x)
    | Many (x, xs) -> Many (f x, map f xs)

// andMap returns true iff a predicate is true for every element in a non-empty list
let forall f xs =
    fold (&&) true (map f xs)

// Given a TermSequence of Functions, heads returns a TermSequence containing the heads of the argument lists
let heads xs =
    map (function
        | Function (name, t::ts) -> t //Function (name, [t])
        | unrecognised -> failwith (sprintf "heads can only process a Function, not %A" unrecognised)) xs

// tails returns a list of Functions whose lists are the tails of the original Functions when given a term sequence (of Functions)
let tails xs =
    map (function
        | Function (name, t::ts) -> Function (name, ts)
        | unrecognised -> failwith (sprintf "tails can only process a Function, not %A" unrecognised)) xs

// hasMap returns true iff the substitution maps a term sequence
let hasMap key substitution =
    List.exists (fun e -> fst e = key) substitution

// findMap returns the map for a term sequence in a substitution
let findMap term_sequence substitution =
    List.find (fun e -> fst e = term_sequence) substitution

// hasVar returns true iff the substitution contain a mapping to a logical variable
let hasVar theta =
    List.exists (function
        | _, Var _ -> true
        | _        -> false) theta

// base_name turns a prefix and a number into a Var
let var base_name n =
    Var (sprintf "%s%d" base_name n)

// allEqual returns true if all elements in the sequence are equal.
let allEqual = function
    | One _ -> true
    | Many (x, xs) -> forall ((=) x) xs

let functionUnifiable name1 arity = function
    | Function (name2, args2) -> name2 = name1 && args2.Length = arity
    | _                       -> false

// unifiable returns true if and only if all elements in the sequence are Functions with
// the same name and the same arity.
let allFunctionUnifiable = function
    | Many (Function (f, args), tail) -> forall (functionUnifiable f args.Length) tail
    | _ -> false

// antiUnifyTheta is a helper for antiUnify, returning the anti-unification of a term sequence with
// respect to a substitution map (mapping term sequences to logical variables)
// n contains the number of logical variables found so far: it lets us create fresh logical variables.
// TODO: At the moment, we use logical variables of the form Var "#z0", Var "#z1", Var "#z2", ... This
// will cause problems is a user happens to also use names in this namespace.
let rec antiUnifyTheta (theta: (TermSequence<Term<'a>> * Term<'a>) list) n = function
    // rule 7: all examples the same? return the first
    | One x -> (x, theta, n)
    | Many (t, _) as examples when allEqual examples -> (t, theta, n) 
    // rule 8: recurse into the Function arguments
    | examples when allFunctionUnifiable examples -> 
        let s, theta', n' = antiUnifyTheta theta n (heads examples)
        let tails_au, theta'', n'' = antiUnifyTheta theta' n' (tails examples)
        let au_of_args = match tails_au with 
                         // How can we be sure we only ever have Functions and Vars here?
                         // The result of tails will always be a list of unifiable functions, so this branch will always be hit
                         // This branch always returns a Function, hence tails_au will always be a function
                         | Function (f, ss) -> Function (f, s::ss)
                         //| Var name -> Function (f, (s::[Var name]))
                         | unrecognised -> failwith (sprintf "unrecognised term %A while walking function argument tails" unrecognised)
        au_of_args, theta'', n''
    // rule 9: return the previously mapped logical variable
    | examples when hasMap examples theta -> 
        let mapped_var = snd (findMap examples theta)
        mapped_var, theta, n
    // rule 10: introduce a fresh logical variable
    | examples -> 
        // This hardcoded base name lets us avoid passing in a name generator. A post-process step could always replace these. But what if someone wants to use "#z" prefixed names....!
        let z = var "#z" n 
        z, ((examples, z) :: theta), n + 1
    // The below clause is unneccessary, because the last branch always matches (maybe it didn't use to?)
    //| unrecognised -> failwith (sprintf "There is no rule 11: %A" unrecognised) // Necessary because all the previous matches have when clauses, so we can't rely on F#'s exhaustiveness checking

// preprocess turns all Vars into ground terms by pretending they're constant terms
let rec preprocess = function
    | Function (name, args) -> Function (name, List.map preprocess args)
    | Var name -> Const name
    | x -> x

// postprocess is the inverse of preprocess: it turns "ground" Consts back into ungrounded variables.
let rec postprocess = function
    | Function (name, args) -> Function (name, List.map postprocess args)
    | Const name -> Var name
    | x -> x

// antiUnify returns the least general generalisation of a list of example terms (a term sequence).
let antiUnify examples =
    let processed_examples = map preprocess examples
    let generalised_example, _, _ = antiUnifyTheta [] 0 processed_examples
    postprocess generalised_example