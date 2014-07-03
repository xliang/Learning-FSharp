#light

module Objectville.FSharp.Sample.Recursion

// Recursion: divide a problem into sub-problem of the same type
// as the original, solve those problems and combine the results
// It is often referred to as the divide-and-conquer method


// Retrieve total files from a directory
open System
open System.IO

let rec getFilesFrom folder = 
    let result = new ResizeArray<string>()
    result.AddRange(Directory.GetFiles(folder))
    let subDirs = Directory.GetDirectories(folder)
    subDirs |> Seq.iter(fun i -> result.AddRange(getFilesFrom i))

    result



// Sum a list recursively

let tempList = [1 .. 1000000]

// Non Tail Recursion Version 
let rec sumList lst = 
    match lst with
    | [] -> 0
    | hd :: tl -> hd + sumList tl 

sumList tempList


// Tail recursion version 
let sumListTR list  = 
    let rec sumUtility list total = 
         match list with
         | [] -> 0 // initial value
         | hd :: tl ->
            let ntotal = hd + total
            sumUtility tl ntotal // don't need to store in stack
    sumUtility list 0 

// We need to keep data in the stack frame, becasue we need to do some work
// after it is complete

// Tail recursion does not need to store anything in the stack, so that compiler 
// can optimize it. We only need to have one slot in the stack.

// Process a List with recursion 

// Splitting a list into its head and tail, applying the function f 
// to the head and consing it to the list obtained from applying the
// function to the tail

// The recursive call to "map" is not last in this instance, so it's not
// tail-recursive

// Non-Tail Recursive version map  function 
let rec map f list = 
    match list with 
    | [] -> []
    | hd :: tl -> (f hd) :: (map f tl)

// F# can recognize recursive functions and perform a "tail call optimization"
// turning the recursive function into a while-loop that uses no stack space whatsoever.

// The rule for tail-recursion is simple:

//    If the recursive call to the function is the very last thing that 
//    happens in the algorithm, then the function can be made tail-recursive.


// Tail Recursion Version (accumulator pattern)
// factoring out an inner loop that is recursive and threading an 
// accumulator 
// (f hd :: acc) process hd with function f and appened to acc !!

let mapTR f list = 
    let rec loop f list acc = 
        match list with 
        | [] -> acc
        | hd :: tl -> loop f tl (f hd :: acc) // capture a closure function f 
    
    loop f (List.rev list) []

// map is a high order function to transfer the element in a collection
// it return a list of tranfered element.
// what is accomplished by (f hd :: acc)
// loop is the very last thing that happens, so the F# compiler 
// can implement this tail-recursively   

// filter with non-trail recursion

let rec filterN f list = 
    match list with 
    | [] -> []
    | x :: xs -> let xs = (filterN f xs)
                 if f (x) then x :: xs else xs

// filter function with tail recursion 

let rec filterTR filter f list acc = 
    match list with 
    | [] -> List.rev(acc)
    | x :: xs -> let acc = if f (x) then x :: acc else acc 
                 filter f xs acc
filterTR f list []

// non-tail recursion factorial

let rec factorialFunction (n) = 
    if (n <= 1) then
        1
    else
        n * factorialFunction(n-1) // execution after recursion call

// Tail Recursion 

// uses a special case of recursion in which the last instruction executed
// in the method is the recursive call - there is no "pending operation" for 
// the runtime to worry about    

// If we could somehow change the “building up” of the result 
// and use another mechanism to eliminate the need for the runtime 
// to allocate a stack frame per call, we could create a tail recursive function 
// and avoid stack overflow. The standard solution is to use an auxiliary parameter.

// A non-tail recursive function can often be converted to a 
// tail-recursive function by means of an "auxiliary" parameter. 
// This parameter is used to build up the result. The idea is to attempt 
// to incorporate the pending operation into the auxiliary parameter in such a way 
// that the recursive call no longer has a pending operation. 

// (accumulator pattern)

let factorial x = 
    let rec tailRecursiveFactorial x acc = 
      if x <= 1 then 
         acc
      else
         tailRecursiveFactorial (x - 1) (acc * x) // return immediately
         
    tailRecursiveFactorial x 1
    
printfn "%d" (factorial 10)

type Chain = 
    | ChainNode of int * string * Chain
    | ChainEnd of string
    // a member to implement tail recursion
    member chain.Length = 
        let rec loop c acc = 
            match c with 
            | ChainNode (_, _, subChain) -> loop subChain (acc + 1)
            | ChainEnd _ -> acc
 

// A function using two recursive function cannot use Tail recurision 

//let rec progsum x = 
//    if x <= 1 then 1
//    else 
//        x + progsum (x -1)


// Continuation Pattern

// Instead of passing the accumulator, representing the current state
// passing a funciton value, carrying the future execution.
// rather than store "what left" in the stack, you store it in a function (closure)
// It is called Continuation Passing Style (CPS)

// continuation passing in the bit of code that we were executed next
// Think about another type of accumulator, instead of value, accumulate "Next thing to execute"

// Why CPS?
// Compilers use a more thorough CPS transformation to produce an intermediate form
// amenable to many analysis. UI framework use CPS to keep the UI reponsive while
// allowing nonlinear program interaction, web servers use CPS to allow computing 
// to flow asynchronously across page

// Instead of returning a value from a function, the value is passed to the code that
// continue the computation. 

// Continuations are function objects, which are allocated on the garbage-collected heap

// Sample 0: Simple sample

let square_func x = x * x 
let square_cps x cont = cont (square_func x )
let result = square_cps 4 (fun x -> x)


// Sample 1: Print a list in revere 

let printListRev list =
    let rec printListRevTR list cont =
        match list with
        // When reaching an empy list, execute the continuation (all of the functions...)
        | [] -> cont()
        // For other lists, add printing the current
        // node as part of the continuation.
        | hd :: tl ->
            printListRevTR tl (fun () -> printf "%d " hd
                                         cont() )

    printListRevTR list (fun () -> printfn "Done!")
    
printListRev [1 .. 10]    

// Sample 2: Process a Binary Tree

// Binary Tree without continuous
type BinTree<'a> =
     | Node of 'a * BinTree<'a> * BinTree<'a>
     | Empty
     
 let rec iter f binTree =
     match binTree with
     | Empty -> ()
     | Node (x, l, r) ->
         f x
         iter f l // NOT in tail position
         iter f r // In tail position

// Binary Tree with continuations

// linearize use continuation to convert the binary tree into ContinuationStep object
// each node is converted into a Step union tag in addition to breaking the recursive
// calls to the left and right subtrees in lambdas
// Finally, the processSteps function breaks apart the ContinuationStep object to 
// actually perform the iter operation on the binary tree

    type ContinuationStep<'a> = 
        | Finished 
        | Step of 'a * (unit -> ContinuationStep<'a>)
    
    let iter2 f binTree = 
        let rec linearize binTree cont = 
            match binTree with
            | Empty -> cont()
            | Node(x, l, r ) ->
                Step(x, (fun () -> linearize l (fun() -> linearize r cont)))
    
        let steps = linearize binTree (fun () -> Finished)

        let rec processSteps step = 
            match step with
            | Finished -> ()
            | Step (x, getNext)
                -> f x 
                    processSteps (getNext())    
    
        processSteps steps             


// Sample 3: Process a tree

type IntTree = 
    | Leaf of int
    | Node of IntTree * IntTree

// Normal Recursive approach

let rec sumTree (tree) = 
    match tree with
    | Leaf (n) -> n
    | Node (l, r) -> sumTree (l) + sumTree (r)

// Continuations Approach

let rec sumTreeCont tree cont = 
    match tree with
    | Leaf (num) -> cont(num)
    | Node (left, right) ->
        sumTreeCont left (fun leftSum ->
            sumTreeCont right (fun rightSum -> 
                cont (leftSum + rightSum)))

// Combine an accumulator with an explicit continuation

let rec sumTreeTRCont acc tree cont = 
    match tree with
    | Leaf (num) -> cont (1 + acc)
    | Node (left, right) -> 
        sumTreeTRCont acc left (fun accLeftSize ->
        sumTreeTRCont accLeftSize right cont)

let size tree = sumTreeTRCont 0 tree (fun x -> x) // pass identity function

// 1. start with an accumulator with 0
// 2. traverse the left spine of the tree till a Leaf is found, building up a 
// continuation for each Node for each along the way
// 3. When a Leaf is encountered, the continuation from the previous Node is called
// with accLeftSize increase by 1. The continuation makes a recursive call to 
// sumTreeTRCont for its right tree, passing the continuation for the second-to-last 
// node along the way
// 4. When all is done, all of the left and right trees have been explored, and the final
// result is delivered to the (fun x -> x) continuation


// Sample 4:  simple Continuation sample

let max m n f = if m > n then f m then f n

max 2 3 (fun number -> printfn "%d" number)


// Sample 5: Process List Length

// Normal Recursion
let rec  length = function
  | [] -> 0
  | _ :: t -> 1 + length t

// Tail Recursion
let  length_tail lst = 
  let rec length_acc l acc =
    match l with
    | [] -> acc
    | _::t -> length_aux t (1 + acc)
  
  length_acc lst 0

// Continuation approach

let  length_cps lst =
  let rec length_cont l cont =
    match l with
    | [] -> cont 1 // initial value
    | _::t -> length_cont t (fun x -> cont(1 + x))
  
  length_cont lst (fun x -> x)

// Sample 6: Map with Continuation

let rec map cont f = function 
     | [] -> cont []
     | x :: xs -> map (fun l -> cont <| f x::l) f xs

// Sample 7: Process tree with Continuation

type Tree<'a> = 
  | Node of 'a * Tree<'a> * Tree<'a>
  | Leaf of 'a

// Non Trail Recursion version
let rec tree_size = function
  | Leaf _ -> 1
  | Node(_, left, right) -> tree_size left + tree_size right

// Trail Recursion version 
let  tree_size_tail tree =
  let rec size_acc tree acc =
    match tree with
    | Leaf _ -> 1 + acc
    | Node(_, left, right) -> 
        let acc = size_acc left acc // Non Tail Recursion on left
        size_acc right acc          // Tail Recursion on right
  size_acc tree 0

// Continuation version 
let  tree_size_cont tree =
  let rec size_acc tree acc cont =
    match tree with
    | Leaf _ -> cont (1 + acc)
    | Node(_, left, right) ->
        size_acc left acc (fun left_size ->
          size_acc right left_size cont)
  
  size_acc tree 0 (fun x -> x)

// Create an inner function which uses an accumulator, 
// our tree and a continuation function as input.

// Return 1 plus the accumulator if we've reached the leaf of the tree
// Else, we're going to call the function to get the left tree size recursively 
// until we reach the leaf.  We create a continuation to get the right tree size.

// Finally, we call the right tree size while passing in the accumulator and our continuation. 

// Sample 8: Sample to sum up a list

let rec sum0 = function 
    | 1 -> 1
    | n -> n + (sum0 (n - 1))

let sum1 n = 
    let rec f n cont = 
        match n with
        | 1 -> cont 1
        | n -> f (n - 1) (fun n1 -> cont (n + n1))
   
    f n (fun x -> x)

let sum2 = 
    let rec f cont = function 
        | 1 -> cont 1
        | n -> f (fun n1 - cont (n + n1)) (n - 1)


let rec sumList l = 
    let rec loop l acc = 
        match l with
        | h :: t -> loop t (h + acc)
        | [] -> acc

// Sample 10: to get Lenght of a list

let rec lengthList l =
    let rec loop l acc = 
        match l with 
        | h :: t -> loop t (1 + acc)
        | [] -> acc 

let rec reverseList l = 
    let rec loop l acc = 
        match l with
        | h :: t -> loop t ( h :: acc)
        | [] -> acc

// Create a Fold function

let rec Fold combine acc l = 
    match l with 
    | h :: t -> Fold combine (combine acc h) t 
    | [] -> acc


Fold (fun acc e -> e + acc ) 0 [1; 2; 3; 4]
Fold (fun acc _ -> 1 + acc ) 0 [1; 2; 3; 4]
Fold (fun acc e -> e :: acc) [] [1; 2; 3; 4]


// Sample 11: factorial Function

// Factorial Function 
let rec factorial n = 
    if n = 0 then 1
    else
       n * factorial (n -1)

let rec factorialTR n a = 
    if n = 0 then a
    else 
       factorialTR (n - 1) (n * a)

let  fibonacciCPS n =
  let rec fibonacci_cont a cont =
    if a <= 2 then cont 1
    else
      fibonacci_cont (a - 2) (fun x ->
        fibonacci_cont (a - 1) (fun y -> 
          cont(x + y)))
          
  fibonacci_cont n (fun x -> x)

// will raise OutOfMemoryException

let rec factorial n a c = 
    if n = 0 then c a 
    else
       fac (n - 1) (n * a) c

let rec factorialCPS n k = 
    if n = 0 then 
        k (1)
    else
        factorialCPS (n - 1) (fun x -> k (n * x))


// Sample 12: Continuation samples

let greeting() = "hello"

let greeting() c = c "hello"

let add a b = a + b
let add a b c = c (a + b)

let double x = 2 * x
let square x = x * x
let sqrdbl x = double (square x)

let double x c = c (2 * x)
let square x c = c (x * x)
let sqrdbl2 x c = square x (fun y -> double y c)

sqrdbl2 7 (printfn "%i")

// First, sqrdbl now takes a continuation. 
// It passes x to square as usual but can’t just pass the continuation (c) along to square. 
// Instead we have to make a new closure (capturing ‘c’) 
// that will be called with the square of x. This new continuation will 
// then forward that on to double and, since double is the last in the sequence, 
// pass on the original continuation which will be called with the ultimate result. 

// A very strange thing to realize about continuations is that they are essentially closures 
// that represent all that is needed to complete the computation. 
// That is, they are self-contained units that represent the rest of the computation; 
// a very strange idea.

let  treeSizeCont tree =
  let rec size_acc tree acc cont =
    match tree with
    | Leaf _ -> cont (1 + acc)
    | Node(_, left, right) ->
        size_acc left acc (fun left_size ->
          size_acc right left_size cont)
  
  size_acc tree 0 (fun x -> x)