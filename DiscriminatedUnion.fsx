#light

module Objectville.FSharp.Sample.DiscriminatedUnion

open System

// Discriminated Unions
// In functional programming, discriminate unions often are used to replace
// single-interitance hierarchies. 

// Option (Some/None) is discriminated unions
type option<'T> = 
        | Some of 'T
        | None


// Scheduler sample
type Scheduler = 
    | Never
    | Once of DateTime
    | Repeatedly of DateTime * TimeSpan

let tomorrow = DateTime.Now.AddDays(1.0)
let noon = new DateTime(2008, 8, 1, 12, 0, 0)
let daySpan = new TimeSpan(24, 0, 0)

let schedule1 = Never
let schedule2 = Once(noon)
let schedule3 = Repeatedly(noon, daySpan)

// pattern matching 
let getNextOccurrence(schedule) = 
    match schedule with
        | Never -> DateTime.MaxValue
        | Once(eventDate) -> 
            if (eventDate > DateTime.Now) then eventDate
            else DateTime.MaxValue
        | Repeatedly(startDate, interval) ->
            let secondsFromFirst = (DateTime.Now - startDate).TotalSeconds
            let q = secondsFromFirst / interval.TotalSeconds
            let q = max q 0.0
            startDate.AddSeconds
                (interval.TotalSeconds * (Math.Floor(q) + 1.0))

type OSObject = 
     | File of string
     | Process of string * int
     | Unknown

// DUs enable us to treat the discriminators polymorphically

let makeOSObject objtype =
    if objtype = "file" then File("")
    else if objtype = "process" then Process ("", 0)
    else Unknown
    
// Recursive Definitions
// A powerful feature of DUs is that they can include recursive references.
// This enables us to define data structures in a recursive manner. 

type Tree<'a> = 
     | Leaf of 'a
     | Node of Tree<'a> * Tree<'a>
     
// How to build a tree structure

let t = Node(Node(Leaf(100), Leaf(200)), Leaf(50))

// How to walk through

let walkTree tree =
    let rec loop depth tree =
        let spacer = new string(' ', depth)
        match tree with
        | Leaf(value) ->       
            printfn "%s |- %A" spacer value
        | Node(tree1, tree2) ->
            printfn "%s |" spacer
            loop (depth + 1) tree1
            loop (depth + 1) tree2
    loop 0 tree
// Binary Tree

type BinaryTree =
    | Node of int * BinaryTree * BinaryTree
    | Empty // Union Case
    
let rec printInOrder tree =
    match tree with
    | Node (data, left, right)
      -> printInOrder left 
         printfn "Node %d" data
         printInOrder right    
    | Empty 
      -> ()

let binTree = 
    Node (2, 
        Node (1, Empty, Empty),
        Node (4,
            Node (3, Empty, Empty),
            Node (5, Empty, Empty)
             )
          )      
          
printfn "print out tree"

printInOrder binTree          


let PrintFirstStartsWithM l =
    let answer = List.tryFind(fun (s : string) -> s.StartsWith("M")) l
    match answer with
        | Some (s) -> printfn "%s" s
        | None -> printfn "list had no strings starting with M"

printfn "print out week day"

PrintFirstStartsWithM ["Sunday"; "Monday"; "Tuesday"]
