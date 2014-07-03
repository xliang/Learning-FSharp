#light

module Objectville.FSharp.Sample.Algorithm.Sort

// Quick Sort
// divide and conqure O(n logn)

// algorithm QuickSort (list)
//     Pre:  list <> null
//     Post: list has been sorted into values
//     if list.Count == 1
//         return list
//     end if
//     pivot <- MedianValue (list)
//     for i <- 0 to list.Count - 1
//        if list[i] == pivot
//           equal.Insert(list[i])
//        end if
//        if list[i] < pivot
//            less.Insert(list[i])
//        end if
//        if list[i] > pivot
//            greater.Insert(list[i])
//        end if
//    end for
//    return Concatenate(QuickSort(less), equal, QuickSort(greater))
//    end QuickSort

let rec qsort list = 
    match list with 
        | [] -> []
        | x :: xs ->
            let smaller = qsort (xs |> List.filter(fun e -> e <= x))
            let larger = qsort (xs |> List.filter(fun e -> e >= x)) 
            smaller @ [x] @ larger

qsort [3;5;1;4;2]

let rec qsort2:int list -> int list = function
    | [] -> []
    | x::xs -> let smaller = [for a in xs do if a<=x then yield a]
               let larger =  [for b in xs do if b>x then yield b]
               qsort smaller @ [x] @ qsort larger

let rec qsort3 = function
    | [] -> []
    | x::xs -> let smaller,larger = List.partition (fun y -> y<=x) xs
               qsort smaller @ [x] @ qsort larger

// using currying for operator

let rec qsort4 = function
| [] -> []
| (x:int) :: xs ->
    let smaller = List.filter ((<=) x) xs
    let larger  = List.filter ((>=) x) xs
    qsort smaller @ [x] @ qsort larger