#light

module Objectville.FSharp.Sample.FileProcessing

open System
// parse data line
let convertDataRow (csvLine : string) = 
    let cells = List.ofSeq(csvLine.Split(','))
    match cells with
        | title::number::_ -> 
          let parseNumber = Int32.Parse(number)
          (title, parseNumber)
        |_ -> failwith "Incorrect data format!"

// parse data lines recursively
let rec processDataLines (lines) = 
    match lines with
    | [] -> []
    | headerLine :: reminingLines ->
        let headerCells = convertDataRow(headerLine)
        let reminingCells = processDataLines(reminingLines)
        headerCells :: reminingCells // return tuple list
