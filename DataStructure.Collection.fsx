#light


module Objectville.FSharp.Sample.DataStructure.Collection

// Array



// ------------------------------------------------------

// List 

// generate a list

let list1 = [for i in 1..10 -> i * i]

let list2 = [for i in 1..10 do yield i + i]

let list3 = List.init 10 (fun n -> 100 * n) //  n starts from 0
 
// cons (::) add to the head of a list

let baseket0 = []
let baseket1 = "fruit" :: []
let baseket2 = "banana" :: baseket1

// recursive

let rec sum list = 
    match list with 
    | head :: tail -> head + sum tail
    | [] -> 0




//---------------------------------------------------------------------
// Sequence 

open System.IO

// yield  generates a single element
// yield! generate a collection of elements

let rec allFiles dir = 
     seq {
          for file in Directory.GetFiles(dir) -> file
          for subdir in Directory.GetDirectories dir do 
              yield! (allFiles subdir) }

allFiles @"c:\temp" |> Seq.iter(printfn "%s")

// when interacting with external data source that can change out from under you, 
// you want to make sure you get the latest-and-greatest data from these source 
// via sequences, consider using Seq.delay

let rec allFilesDelay dir = 
    Seq.delay (fun () -> 
        let files = Directory.GetFiles(dir)
        let subdirs = Directory.GetDirectories(dir)
        Seq.append
            files
            (subdirs 
                |> Seq.map allFilesDelay
                |> Seq.concat
            )
        )
allFilesDelay @"c:\temp" |> Seq.iter(printfn "%s") 

// Seq.concat: string together, a set of enumerables into a single enumerable

// Seq.append: add one to the end of the other


                     