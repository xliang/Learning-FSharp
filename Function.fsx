
#light

module Objectville.FSharp.Sample.Functions

// Instead of thinking about a function "returning" a value, it helps to think 
// about a function "resolving to a value" or "evaluating to a value". 


// let functionName [parameter list] = functionBody

// Function takes 0 params, and evaluates to a value
let foo = 123

// Single parameter
let f x = x + 1

// Optional Parentheses

let g (x) = x + 1

// !! In F#, function only ever take a single parameter and return a single value.
// let sumVal x y = x + y
// int -> (int -> int)
// the arrow "->" is right associative
// which means that this type can be interpreted as "int -> (string -> unit)"

// ** function take an integer arugment return a function which takes a string and return unit**

// all of F# function are Partial Function
// let (sumVal x) y = 
// Function application associate to the left, means that this call to call a one-argument
// funciton sumVal, that takes an int and return a new one-argument function which 
// take another argument y


// let sumVal = (fun x -> (fun y -> x + y))


// declare a function
let addTen = fun x -> x + 10

// shorter version
let addTen1 x = x + 10

// lambda function

let cube = fun n -> n * n * n

let foo1 (n : int) (f : int -> int) = f (n)

let x = foo1 5 (fun n -> n * n)

let y = foo1 5 (fun n -> n * n * n)

let z = foo1 100 (fun n -> n / 2)

// Symbolic Function
// put the parentheses around the symbol to pass around to a high order function
let a = List.fold (+) 0 [1..10]

// Mapping

// Create an array 0..9 using a lambda function to initalize the elements
let nums = Array.init 10 (fun n -> n)

// Apply square function to each element of nums
let squares = Array.map (fun n -> n * n ) nums

// Folding
let sum = Array.fold( fun acc element -> acc + element) 0 nums

// Unfolding
// List or Seq

// Filtering

let evens = Array.filter( fun item -> item % 2 = 0) nums

// Zipping

// Pipeline

let processes = 
    System.Diagnostics.Process.GetProcesses()
    |> Array.map(fun a  -> a.ProcessName)
    |> Array.filter(fun a -> a <> "taskeng")
    
// Function Composition

let h = f >> g
let result = h 5

// Partial Application and Currying

let add x y = x + y
let increment = add  1
let value = increment 41

// Closure


// Recursive
let rec factorial a = 
    if a <= 1 then
        1
    else
        a * factorial(a - 1)

// High Order Function
// Separate function from data
let rec comb f = f (fun x -> comb f x)
let progworker f x =
    if x <= 1 then 1
    else x + f (x + 1)

// use partial application to wrap up the combinator call
// let progCombinator x = comb progworker x
// explain: 
// comb is a high order function that takes another function as parameter and
// return a function taking one argument. 
// a function taking one parameter and returning something of same type as the parameters
// Sample:
// let factworker f x = 
//    if x <= 1 then 1
//    else f(x -1) * x
// let factcombinator x = comb factworker x

// fold
// applies a given function argument to a set of elements, accumulating a result
// left fold and right fold
// fold_left (+) 0 xs => ((0 + 1) + 2) + 3
// fold_right (+) xs 0 => 1 + ( 2 + (3 + 0))


// Function Combinators
// 1. f << g  is equivalent to f (g x)
let apply_2 f = f << f 
// apply_2 ((*) 2 ) 5 -> 20 
// Can use to inject new code in between function calls


// 2. f >> g  is equivalent to g (f x)
// 3. x |> f  is function applicaiton written in reverse, equivalent to f x



// Continuations
// Continuation Passing Style or CPS

let mul x y fn = fn (x * y)

// continuation function
let addone n = n + 1

// capture the final result
let finalResult = mul 3 5 addone


// Lazy Evaluation

// Operator Overloading

let (+) (x : int) (y : int) = x + 2 * y










