#light

module Objectville.FSharp.Sample.BasicType

open System.Windows.Forms

//-------------------------Record --------------------------//

// Append members directly to the type declaration, when the type is simple enough
// Usual declaration of F# record type 

type Rect =
    { Left   : float32
      Top    : float32
      Width  : float32
      Height : float32 }
    /// Creates a rectangle which is deflated by 'wspace' from the 
    /// left and right and by 'hspace' from the top and bottom
    member x.Deflate(wspace, hspace) =
      { Top = x.Top + wspace
        Left = x.Left + hspace
        Width = x.Width - (2.0f * wspace)
        Height = x.Height - (2.0f * hspace) }
    /// Converts the rectangle to representation from 'System.Drawing'
    member x.ToRectangleF () = 
      RectangleF(x.Left, x.Top, x.Width, x.Height)

//----------------- Distriminated Union --------------------------//

// Original type declaration
type Schedule =
  | Never
  | Once of DateTime
  | Repeatedly of DateTime * TimeSpan

// Calculate the next occurence of an event
let futureOrMaxValue(dt) = 
  if (dt > DateTime.Now) then dt else DateTime.MaxValue

let getNextOccurrence(schedule) =
  match schedule with
  | Never -> DateTime.MaxValue
  | Once(eventDate) -> futureOrMaxValue(eventDate)
  | Repeatedly(startDate, interval) ->
      // How many times will it occur until today?
      let q = (DateTime.Now - startDate).TotalSeconds / interval.TotalSeconds
      // Only consider occurrences after start date
      let q = max q 0.0
      // Calculate first occurrence after today
      startDate.AddSeconds(interval.TotalSeconds * (Math.Floor(q) + 1.0))

// Type extension
type Schedule with
  // Member just calls the function
  member x.GetNextOccurrence() = getNextOccurrence(x)
  member x.OccursNextWeek() = 
    getNextOccurrence(x) < DateTime.Now.AddDays(7.0) 

// Listing 9.6 Working with schedule using members
let sch1 = Repeatedly(DateTime(2000, 7, 24), TimeSpan(365, 0, 0, 0))
sch1.OccursNextWeek()

let sch2 = Never
sch2.OccursNextWeek()

// Pass value usig IDisposable

let changeColor clr = 
  // Store the original color
  let orig = Console.ForegroundColor
  // Set the new color immediately
  Console.ForegroundColor <- clr
  // ??Create 'IDisposable' value
  // TODO: Why??
  { new IDisposable with
      member x.Dispose() = 
        // Restore the original color inside 'Dispose' 
        Console.ForegroundColor <- orig }

let hello () = 
  // Color is changed to red
  // use is similar to let keyword, 
  // the difference is that the F# compiler automatically adds a call
  // to Dispose method at the end of the function

  use n = changeColor ConsoleColor.Red
  Console.WriteLine("Hello world!")
  // Original color is restored
  
Console.WriteLine("Calling hello...")
hello ()
Console.WriteLine(".. done!")

// Discriminated Unions
// In functional programming, discriminate unions often are used to replace
// single-interitance hierarchies. 

// Option (Some/None) is discriminated unions
type option<'T> = 
        | Some of 'T
        | None

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

// Arithmetic expression evaluator

type Expr = 
    | Number of int
    | Sum of Expr * Expr
    | Diff of Expr * Expr
    | Prod of Expr * Expr
    | Div of Expr * Expr

let rec Eval = function 
    | Number i -> i
    | Sum (e1, e2) -> Eval e1 + Eval e2
    | Diff (e1, e2) -> Eval e1 - Eval e2
    | Prod (e1, e2) -> Eval e1 * Eval e2
    | Div (e1, e2) -> Eval e1 / Eval e2

let result = Prod (Sum (Number 11, Number 15), Diff (Number 40, Number 34))

// A better way to use binary operation

type Expr2 =
    | Number of int
    | BinOp of (int -> int -> int) * Expr2 * Expr2 // Define the function siganature here
with 
    static member Sum (e1, e2) = BinOp ((+), e1, e2)
    static member Diff (e1, e2) = BinOp((-), e1, e2)
    static member Prod (e1, e2) = BinOp((*), e1, e2)
    static member Div (e1, e2) = BinOp((/), e1, e2)

let rec Eval2 = function 
    | Number i -> i
    | BinOp (f, e1, e2) -> f (Eval2 e1) (Eval2 e2)

Expr2.Prod(Expr2.Sum (Number 11, Number 12), Expr2.Diff(Number 45, Number 34))
|> fun i -> 
     i |> Eval2 |> printf "The result is %d\n";;
