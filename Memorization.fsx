#light

module Objectville.FSharp.Sample.Memorization

open System.Collections.Generic

let addSimple (a, b ) = 
    printfn "adding %d + %d" a b
    a + b

let add = 
    let cache = new Dictionary<_, _>()
    (fun x ->
        match cache.TryGetValue(x) with // Good way to use pattern match
        | true, v -> v
        | _ -> let v = addSimple(x) // Calculate the value
               cache.Add(x, v)      // Store in the cache
               v)

let memorize (f) = 
    let cache = new Dictionary<_, _> ()
    (fun x -> 
        match cache.TryGetValue(x) with
        | true, v -> v
        | _ -> let v = f(x)
               cache.Add(x, v)
               v)

// mutually recursive function

let rec factorial (x) = 
    printf "factorial (%d); " x
    if (x <= 0) then 1 else x * factorial (x - 1)

let factorialMem = memorize factorial

// Correct version 

let rec factorial2 (x) = memorize (fun x -> 
        printfn "Calculating factorial (%d)" x
        if (x <= 0) then 1 else x * factorial2(x - 1))


let factorialMem2 = memorize factorial2




