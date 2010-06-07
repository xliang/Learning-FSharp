
#light

module Objectville.FSharp.Sample.DataType

open System

// let bind a symbol to a data value
// let introduce a new symbol scope
// n is not a value, let binds the symbol n to the integer 123

let n = 123

// Enum
type Color =
    | Red = 0 | Green = 1 | Blue = 2

let myColor = Color.Blue


// mutable

let mutable x = 345
x <- 456

// Reference Call

let daughter = ref "Helen"

daughter := "Helena"

// Access a reference cell with operator (!)
printfn "daughter name is %s" !daughter


// Generic Types

type 'a Stack =
    | EmptyStack
    | StackNode of 'a * 'a Stack
    
let iStack = StackNode(3, StackNode(5, EmptyStack))
let fStack = StackNode(2.0, StackNode(1.0, EmptyStack))
let sStack = StackNode("world", StackNode("hello", EmptyStack))             

// Tuple
let tuple1 = (1, 2, 3)        
// Extract the data from Tuple
let a, b, c = tuple1
printfn "a is %d" a


// Records 
// Records are tuples with names components, know as fields

type record1 = {x : float; y : float}
let zero = { y = 0.0; x = 0.0}
printfn "%f" zero.y

// use with notation to create a new record from an existing record

let x_axis = {zero with x = 1.0}
printf "%f" x_axis.x


/// Array

let evens = [|2; 4; 6; 8|]

// Explicitly Defining an Array's Type

let kids1: string[] = Array.zeroCreate 3
let kids2: string array = Array.zeroCreate 3
let kids3 = Array.zeroCreate<string> 3

// Arrays are implicitly mutable data structure

kids1.[0] <- "Jessica"
kids2.[1] <- "kimberly"
kids3.[2] <- "melissa"

// Covert to list by prepending elements using the cons operator:

let to_list a = 
    Array.foldBack (fun h t -> h ::t) a []

/// List 

// List Generator
let squares = [for i in 1..10 -> i * i] // squares of 1 - 10
let evensq = [for i in 1..10 do if i % 2 = 0 then yield i * i] 


// Create List using Recursion

let rec arrayToList ( a : int array) n =
    if (n = a.Length) then []
    else a.[n] :: arrayToList a (n + 1)


[1..10] |> List.iteri(fun i j -> printfn "%d - %d" i j)

// List comprehension
let primesUnder max =
     [
         for n in 1 .. max do 
             let factorsOfN = 
                 [
                     for i in 1 .. n do 
                         if n % i = 0 then
                             yield i 
                 ]
                 
             if List.length factorsOfN = 2 then
                 yield n
     ]    


// List mapping

let planets = [("mars",4); ("venus",2); ("mercury",1); ("earth",3)]
let planetOrds = [1; 2; 3; 4]
// iterate plantOrds to query planets' second data value
let names =
    planetOrds
    |> List.map (fun po -> fst(List.find(fun p -> snd p = po) planets))

// String Sample

open System

let stringJoin = 
    String.Join(",", [|"1", "2", "3"|])