#light

module Objectville.FSharp.Sample.PatternMatching
(*
    Sample includes Pattern matching and active pattern
*)

open System



//----------------------------Pattern Match-------------------------------//

// Patterns are rules for transforming data. we use patterns throughout F#
// to the following types of things:

// a. compare data with a logical structure
// b. decompose data into its constituent parts
// c. extract information from constructs in various ways.

// Matching Anything - The Wildcard Pattern
// Use wildcard to prevent F# from needing to throw a MatchFailureException

match  3 with
   | _ -> printfn "wildcard matches anything."
      
// pattern matching shortcut syntax

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
// It is an alternative functional representation of match
// it does not use match. Instead, this form of pattern matching uses the 
// keyword function to include a lambda expression ("pattern matching function") 

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

//------------------------------------Active Pattern--------------------------//

// Active pattern allow you to view arbitrary values through a different len
// much the same way as you gave a different underlying representation to expression

// use active pattern to 
// 1. remove "when" guard in pattern match
// 2. make pattern match more expressive  

// Single case Active Pattern

// Convert one type to another
let (|F2C|) f =
    (f - 32.0) * (5.0 / 9.0)    

// f is used as the input to the active pattern
// and c is the output

// F2C is called in the pattern matching function
// Convert from f to c
let test (f: float) =
     match f with
     | F2C c when c > 30.0 -> printfn "it's hot %f" c
     | F2C c when c > 20.0 -> printfn "it's temperate %f" c
     | F2C c when c > 10.0 -> printfn "it's cool %f" c
     | _ -> printfn "it's getting cold!"


// Multi-case active pattern
// Covert the input data into one of the several values
// Just like to convert input data into discriminated union

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

// This active pattern divides all strings into their various meanings.
// The types can contains values !!

let (|Paragraph|Sentence|Word|WhiteSpace|) (input : string) =
        let input = input.Trim()
        
        if input = "" then
            WhiteSpace
        elif input.IndexOf(".") <> -1 then
            // Paragraph contains a tuple of sentence counts and sentences.
            let sentences = input.Split([|"."|], StringSplitOptions.None)
            Paragraph (sentences.Length, sentences)
        elif input.IndexOf(" ") <> -1 then
            // !! Sentence contains an array of string words
            Sentence (input.Split([|" "|], StringSplitOptions.None))
        else
            // Word contains a string
            Word (input)
 
// Count the number of letters of a string by breaking it down

let rec countLetters str =
    match str with
    | WhiteSpace -> 0
    | Word x     -> x.Length
    | Sentence words
        -> words 
           |> Array.map countLetters 
           |> Array.sum
    | Paragraph (_, sentences)
        -> sentences
           |> Array.map countLetters  
           |> Array.sum



// Partial Case Active Pattern

// It is a special case of Active Pattern, it does not always convert to a type
// It returns option type, either Some or None.

let (|ToInt|_) x = 
    let success, result = Int32.TryParse(x)
    if (success) then Some(result)
    else    None

let (|ToFloat|_) x = 
    let success, result = Double.TryParse(x)
    if (success) then Some(result)
    else    None

let convertValue x = 
    match x with 
    | ToInt b -> printfn "%s is a integer" b
    | ToFloat b -> printfn "%s is a float" b
    | _ -> printfn "cannot convert to int or float"
       

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

// Active Pattern is a function

let (|PrimeEnding99|_|) (num: int) = // pattern recognizer
    { 2 .. num |> float |> Math.Sqrt |> int } // Sequence Expression??
    | Seq.tryFind (fun i -> num % i = 0)
    |> function 
        | None -> 
            if (num + 1) % 100 = 0 then
                num |> Some
            else
                None
        | _ -> 
            None

let _ = 
    match 199 with 
        | PrimeEnding199 _ ->
              printf "199 is a prime ending with 99\n"
        | _ -> 
              printf "199 is a not a prime\n"