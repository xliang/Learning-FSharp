#light

module Objectville.FSharp.Sample.PatternMatching

open System

// Patterns are rules for transforming data. we use patterns throughout F#
// to the following types of things:

// a. compare data with a logical structure
// b. decompose data into its constituent parts
// c. extract information from constructs in various ways.

// Matching Anything - The Wildcard Pattern
// Use wildcard to prevent F# from needing to throw a MatchFailureException

match  3 with
   | _ -> printfn "wildcard matches anything."
      
// Another shortcut syntax

//let something = function
//    | test1 -> value1
//    | test2 -> value2
//    | test3 -> value3


// In the following example, we create a recursive function 
// that counts the number of elements in the given list. 
// Although contrived, this example helps demonstate the empty-list and cons patterns:

let rec count lst =
    match lst with
    | [] -> 0                   // empty list -> 0 elements
    | [_] -> 1                  // any one thing -> 1 element
    | [_;_] -> 2                // any two things -> 2 elements
    | _ :: t -> count t + 1     // cons pattern. count recursively.   

// Let’s take a look at another list-based example that uses the cons pattern 
// to sum up all the numbers in the collection:

let rec sum theList =
    match theList with
    | [] -> 0  
    | h :: t -> h + sum t    

// Using pattern matching in conjunction with recursive list processing 
// is a common idiom in F# programming

let rec pairs = function 
    | h1 :: (h2 ::_ as t) -> (h1, h2) :: pairs t
    | _ -> []

// Guarded Pattern
// When pattern: filter out only the non-negative numbers from a list
// Guarded pattern should be used only when necessary

let rec positive = function 
    | [] -> []
    | h :: t when h < 0 -> positive t
    | h :: t -> h :: positive t

// Or pattern: 
let is_sign = function 
    | -1 | 0 | 1 -> true
    | _ -> false;

// Erroneous Patterns
// Alternative patterns in a match case must share the same set of variable bindings.

let product a b = 
    match a, b with
    // | a, 0 | 0, b -> 0 (Cannot compile, different set of variables.
    | a, b -> a * b
// Can be correct by anonymous _ patterns
let productCorrect a b = 
    match a, b with
    | _, 0 | 0, _ -> 0 
    | a, b -> a * b

// Parallel Pattern 
// It is often applied to several different values in a single function

let unbox3 a b c =
    match a, b, c with
    | Some a, Some b, Some c -> Some (a, b, c)
    | _ -> None

// Pattern Matching can be used for evaluate if the function returns null

let readInput() =
    let s = Console.ReadLine()
    // Try to parse the input
    let (succ, num) = Int32.TryParse(s)
    if (succ) then
        // Return value using 'Some' discriminator
        Some(num)
    else
        // Return undefined value using 'None'
        None;;

let testInput() =
    let inp = readInput()
    // We cannot use the value directly!
    // Check for alternatives using pattern matching
    match inp with
    | Some(v) ->
        // Branch for correct input
        printfn "You entered: %d" v
    | None ->
        // Branch for undefined input
        printfn "Incorrect input!";;

// Test the function interactively
testInput()

// Active Pattern

let (|F2C|) f =
    (f - 32.0) * (5.0 / 9.0)    

// f is used as the input to the active pattern
// and c is the output

let test (f: float) =
     match f with
     | F2C c when c > 30.0 -> printfn "it's hot %f" c
     | F2C c when c > 20.0 -> printfn "it's temperate %f" c
     | F2C c when c > 10.0 -> printfn "it's cool %f" c
     | _ -> printfn "it's getting cold!"

// multi-case active pattern
let (|Child|Teen|Adult|Senior|) age =
    if age < 13 then Child
    elif age >= 13 && age < 18 then Teen
    elif age > 18 && age < 60 then Adult
    else Senior

let rec showPeople p =
    let mutable tail = []
    match p with
    | [] -> printfn "<done>"
    | h :: t ->
        printf "%d = " h
        tail <- t
        match h with
        | Child  -> printfn "child";   
        | Teen   -> printfn "teen";    
        | Adult  -> printfn "adult";   
        | Senior -> printfn "senior";  
        showPeople tail

// Partial Active Pattern


// Parameterized Active Pattern

let (|StringMatches|_|) (mytarget : string) (mystring : string)  =
    if (mystring.Contains(mytarget)) then Some(true) else None    

// str serves as input to the StringMatches active pattern. 
// the match's test-expression always binds to the last (right-most) parameter
// in the active pattern. This means that in this example, str binds to tha mystring argument
// target binds to mytarget string and the result variable binds to the active pattern's
// Some/None return value

let stringContains str target =
    match str with
    | StringMatches target result -> printfn "%s contains %s" str target
    | _ -> printfn "Substring not found"    
