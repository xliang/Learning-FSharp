#light

// Three implementation of Tail Recursion to process List
// mapl_tr1: This function captures the function “f” (a closure) 
// from within the inner loop.

// mapl_tr2: This function explicitly passes the function “f” 
// to the inner loop to test if a closure slows things down

// mapl_tr3: This function creates tuples when passing values to 
// the inner loop and recursing to test if tuples are faster than 
// partial function applications

module Objectville.FSharp.Sample.Recursion.List


open System
open System.Diagnostics

let press_enter () =
  printfn "Press Enter to continue…"
  Console.ReadLine() |> ignore

let time f =
  let sw = new Stopwatch()
  sw.Start()
  f ()
  sw.Stop()
  sw.Elapsed.TotalMilliseconds

let time_iter n f =
      [0 .. n]
      |> List.map (fun _ -> time f)
      |> List.average


module List =
  let mapl_tr1 f l =
    let rec loop l2 acc =
      match l2 with
      | [] -> acc
      | h::t -> loop t ((f l2)::acc)
    loop l []

  let mapl_tr2 f l =
    let rec loop f2 l2 acc =
      match l2 with
      | [] -> acc
      | h::t -> loop f2 t ((f2 l2)::acc)
    loop f l []


  let mapl_tr3 f l =
    let rec loop (f2, l2, acc) =
      match l2 with
      | [] -> acc
      | h::t -> loop (f2, t, ((f2 l2)::acc))
    loop (f, l, [])
    

let main () =
  printfn "Tail Recursion Test"
  let run_test m =
    time_iter 10 (fun () -> [1 .. 100000] |> m List.head |> ignore)
  let methods = [List.mapl_tr1; List.mapl_tr2; List.mapl_tr3]
  let names = ["mapl_tr1"; "mapl_tr2"; "mapl_tr3"]
  
  methods
    |> List.map run_test
    |> List.zip names
    |> List.iter (fun i -> printfn "%s: %f ms?" (fst i) (snd i))

  press_enter ()
      
main()
