module AntiUnification

type Symbol = string

type Term<'a> =
    | Function of Symbol * Term<'a> list
    | Val of 'a
    | Var of string
    | Const of string

type TermSequence<'a> = Term<'a> list

// ('a -> bool) -> ['a] -> bool
let andMap f xs =
    List.fold (fun a b -> a && b) true (List.map f xs)

// Given a term sequence (of Functions), return a list of Functions whose contained lists contain the heads of the original Functions
let heads xs: Term<'a> list =
    List.map (function
        | Function (name, [t]) -> t
        | Function (name, t::ts) ->
            Function (name, [t])
        | unrecognised -> failwith (sprintf "heads can only process a Function, not %A" unrecognised)) xs
   
// Given a term sequence (of Functions), return a list of Functions whose lists are the tails of the original Functions
let tails xs =
    List.map (function
        | Function (name, [t]) -> Function (name, [])
        | Function (name, t::ts) ->
            Function (name, ts)
        | unrecognised -> failwith (sprintf "tails can only process a Function, not %A" unrecognised)) xs

let has_map key substitution =
    List.exists (fun e -> fst e = key) substitution

let find_map term_sequence substitution =
    List.find (fun e -> fst e = term_sequence) substitution

// Does the substitution contain a mapping to a logical variable?
let has_var term_sequence theta =
    List.exists (function
        | (term_sequence, Var _) -> true
        | _ -> false) theta

let var base_name n =
    Var (sprintf "%s%d" base_name n)

// At the moment, we use logical variables of the form Var "#z0", Var "#z1", Var "#z2", ....
// Theta maps term sequences to Vars. That is, it accumulates the parts of the examples that differ.
// n contains the number of logical variables found so far: it lets us create fresh logical variables.
let rec antiUnifyTheta examples (theta: (TermSequence<'a> * Term<'a>) list) n =
    printfn "antiUnifyTheta %A" examples
    match examples with
    | [] -> failwith "Cannot invoke antiUnifyTheta with []"
    | t::ts when andMap (fun t' -> t' = t) examples -> (t, theta, n) // rule 7: all examples the same? return the first
    | ((Function (f, args)) as t)::ts when (andMap (function
                                                 | Function (this_name, these_args) -> f = this_name && args.Length = these_args.Length // Function name & arity count!
                                                 | _ -> false) examples) -> // rule 8: recurse into the Function arguments.
        printfn "recursion1 into %A" (heads (t::ts))
        let (s, theta', n) = antiUnifyTheta (heads (t::ts)) theta n
        printfn "recursion2 into %A" (tails (t::ts))
        let (tails_au, theta'', n) = antiUnifyTheta (tails (t::ts)) theta' n
        let au_of_args = match tails_au with
                         | (Function (g, ss)) -> Function (f, s::ss)
                         | Var name -> Function (f, (s::[Var name]))
                         | unrecognised -> failwith (sprintf "recursion2: %A" unrecognised)
        (au_of_args, theta'', n)
    | ts when has_map ts theta -> // rule 9: return the previously mapped logical variable
        let (term_sequence, mapped_var) = find_map ts theta
        (mapped_var, theta, n)
    | ts -> // rule 10: introduce a fresh logical variable
        let z = var "#z" n // This hardcoded base name lets us avoid passing in a name generator. A post-process step could always replace these. But what if someone wants to use "#z" prefixed names....!
        (z, ((ts, z) :: theta), n + 1)
    | _ -> failwith "There is no rule 11" // Necessary because all the previous matches have when clauses

let rec preprocess = function
    | Function (name, args) -> Function (name, List.map preprocess args)
    | Var name -> Const name
    | x -> x

let rec postprocess = function
    | Function (name, args) -> Function (name, List.map postprocess args)
    | Const name -> Var name
    | x -> x

let antiUnify = function
    | [] -> None
    | examples ->
        let processed_examples = List.map preprocess examples
        let (generalised_example, _, _) = antiUnifyTheta processed_examples [] 0
        Some (postprocess generalised_example)