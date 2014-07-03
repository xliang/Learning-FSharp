
#light

module Objectville.FSharp.Sample.ImperativeAndObjectOritented

// Declare class with Functional Way

// All properties are read-only

type ClientF (name, inc) = 
    member x.Name = name
    member x.Income = inc

    member x.WithIncome (v) = 
        new ClientF(name, v)

    member x.Report() = 
        printfn "%s %d" name inc

// Declare class in a Imperative Way

type ClientI (name, inc) = 
    let mutable inc = inc

    member x.Name = name
    member x.Income 
        with get() = inc
        and set(v) = inc <- v
    
    member x.Report() = 
        printfn "%s %d" name inc

// F# Interface declaration

type Client = {
        Name : string; Income : int; YearsInJob : int
        UsesCreditCard : bool; CriminalRecord : bool 
  }

type ClientTest = 
    abstract Check : Client -> bool 
    abstract Report : Client -> unit

// Implicit class declaration
type CoefficientTest(income, years, min) =

  // Local helper functions
  let coeff(client) =
    float(client.Income)*income + float(client.YearsInJob)*years
  let report(client) =
    printfn "Coefficient %f is less than %f." (coeff(client)) min

  // Standard public method of the class
  member x.PrintInfo() =
    printfn "income*%f + years*%f > %f" income years min

  // Interface implementation using helpers
  interface ClientTest with
    member x.Report(client) = report(client)
    member x.Check(client) = coeff(client) < min 

