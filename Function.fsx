
#light

module Objectville.FSharp.Sample.Functions


//--------------Function as Value---------------------//

// Instead of thinking about a function "returning" a value, it helps to think 
// about a function "resolving to a value" or "evaluating to a value". 
// Note: !! Function is Value !!

// let functionName [parameter list] = functionBody

// Function takes 0 params, and evaluates to a value
let foo = 123

// Single parameter
let f x = x + 1

// Optional Parentheses

let g (x) = x + 1

// !! In F#, function only ever take a single parameter and return a single value. !!!

// let sumVal x y = x + y

// int -> (int -> int)
// the arrow symbol "->" is right associative, (read from right to left)
// The function tells us that sumVal is a function that takes an integer parameter (int ->)
// value and evaluates to (return) a new function

// invoke a function is left-associative. This means that we read function invocation
// from left to right in order to group or associate elements. s 

// which means that this type can be interpreted as "int -> (string -> unit)"
// ** function take an integer arugment return a function which takes a string and return unit**

// all of F# function are Partial Function
// let (sumVal x) y = 
// Function application associate to the left, means that this call to call a one-argument
// funciton sumVal, that takes an int and return a new one-argument function which 
// take another argument y


// let sumVal = (fun x -> (fun y -> x + y))

// declare a function using lambda version

let addTen = fun x -> x + 10

// declare a function using let binding 
let addTen1 x = x + 10

// lambda function

let cube = fun n -> n * n * n

// function take another function as argument
let foo1 (n : int) (f : int -> int) = f (n)

let x = foo1 5 (fun n -> n * n)

let y = foo1 5 (fun n -> n * n * n)

let z = foo1 100 (fun n -> n / 2)

// Return a function a 

let adder n =
    fun a -> a + n // return a function

let adder10 = adder 10

let result = adder10 55

let add1 = fun a b -> a + b

// same as 
let add2 = fun a -> fun b -> a + b

//--------------------Pipeline-------------------------------//

let processes = 
    System.Diagnostics.Process.GetProcesses()
    |> Array.map(fun a  -> a.ProcessName)
    |> Array.filter(fun a -> a <> "taskeng")
    
//-------------------Function Composition---------------------//

// Function Combinators

let h = f >> g
let result = h 5

// 1. f << g  is equivalent to f (g x)
let apply_2 f = f << f 
// apply_2 ((*) 2 ) 5 -> 20 
// Can use to inject new code in between function calls

// 2. f >> g  is equivalent to g (f x)
// 3. x |> f  is function applicaiton written in reverse, equivalent to f x


//-----------------Partial Application and Currying------------//

let add x y = x + y
let increment = add  1
let value = increment 41

[1..10] |> List.map ((+) 100) // partial application
//(+) 100 is a function

//------------------------Closure------------------------------//

// contains the value num in the function
// num is captured
let creatAdder num = (fun m -> num + m)

//--------------------High Order Function--------------------------//

let mapFirst f (a, b) = (f(a), b)

let mapSecond f (a, b) = (a, f(b))

let oldPrague = ("Prague", 11880000)

oldPrague |> mapSecond ((+) 13195)

// mapSecond ((+) 13195) is a partial function 

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

// Mapping
// Can be processed using a loop
// Apply square function to each element of nums
let squares = Array.map (fun n -> n * n ) nums
// Symbolic Function
// put the parentheses around the symbol to pass around to a high order function
let a = List.fold (+) 0 [1..10]

// Folding
let sum = Array.fold( fun acc element -> acc + element) 0 nums

// Unfolding
// List or Seq

// Filtering
let evens = Array.filter( fun item -> item % 2 = 0) nums

// Zipping

//------------------------Lazy Evaluation--------------------//

// Operator Overloading

let (+) (x : int) (y : int) = x + 2 * y










