#light 

module Sample.Recursion

// Process a List with recursion 

// Splitting a list into its head and tail, applying the function f 
// to the head and consing it to the list obtained from applying the
// function to the tail
// The recursive call to "map" is not last in this instance, so it's not
// tail-recursive

let rec map f list = 
    match list with 
    | [] -> []
    | hd :: tl -> (f hd) :: (map f tl)

// F# can recognize recursive functions and perform a "tail call optimization"
// turning the recursive function into a while-loop that uses no stack space whatsoever.

// The rule for tail-recursion is simple:

//    If the recursive call to the function is the very last thing that 
//    happens in the algorithm, then the function can be made tail-recursive.

// Tail Recursion (accumulator pattern)
// factoring out an inner loop that is recursive and threading an 
// accumulator 
// (f hd :: acc) process hd with function f and appened to acc !!

let map2 f list = 
    let rec loop f list acc = 
        match list with 
        | [] -> acc
        | hd :: tl -> loop f tl (f hd :: acc)
    
    // loop is the very last thing that happens, so the F# compiler 
    // can implement this tail-recursively    
    loop f (List.rev list) []


let rec factorialFunction (n) = 
    if (n <= 1) then
        1
    else
        n * factorialFunction(n -1) 

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
         tailRecursiveFactorial (x - 1) (acc * x)
         
    tailRecursiveFactorial x 1
    
printfn "%d" (factorial 10)


//let rec progsum x = 
//    if x <= 1 then 1
//    else 
//        x + progsum (x -1)


// Print a list in revere (Continuation Pattern)

let printListRev list =
    let rec printListRevTR list cont =
        match list with
        // For an empy list, execute the continuation
        | [] -> cont()
        // For other lists, add printing the current
        // node as part of the continuation.
        | hd :: tl ->
            printListRevTR tl (fun () -> printf "%d " hd
                                         cont() )

    printListRevTR list (fun () -> printfn "Done!")
    
printListRev [1 .. 10]    

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

// Binary Tree with continuous

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
            