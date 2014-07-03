
#light

module Objectville.FSharp.Sample.AsynchrounousWorkflow

// Asynchronous (Async) Workflows

// Async workflows enable parallel execution, asynchronous execution, 
// and reactive execution – and combinations thereof. 
// Let’s define these terms so that we’re using them consistently:

// 1. Parallel execution means starting several operations at once 
// and waiting for all of them to finish before continuing, 
// e.g., downloading 10 stock quotes “in parallel.”

// 2. Asynchronous execution means to start doing something in the background 
// (could be in parallel, too) and notify the original caller when finished, 
// e.g., calling a Web service.

// 3. Reactive execution means “wait for something to happen” and then respond, 
// e.g., wait for the user to click a button.

// Expression
// let identifier = async { expression }

open System
open System.Net
open Microsoft.FSharp.Control.WebExtensions
open System.Text.RegularExpressions
open System.Threading

let urlList = [ "Microsoft.com", "http://www.microsoft.com/"
                "MSDN", "http://msdn.microsoft.com/"
                "Bing", "http://www.bing.com"
              ]

let fetchAsync(name, url:string) =
    async { 
        try
            let uri = new System.Uri(url)
            let webClient = new WebClient()
            let! html = webClient.AsyncDownloadString(uri)
            printfn "Read %d characters for %s" html.Length name
        with
            | ex -> printfn "%s" (ex.Message);
    }

let runAll() =
    urlList
    |> Seq.map fetchAsync
    |> Async.Parallel 
    |> Async.RunSynchronously
    |> ignore

runAll()

// Another sample
let urls = ["http://www.google.com/"; "http://microsoft.com/"; "http://www.wordpress.com/"; "http://www.peta.org"]

let downloadUrl(url: string) =
    async {
        let wc = new System.Net.WebClient()
        let matches =
            wc.DownloadString(url)
               |> fun html -> Regex.Matches(html, @"http://\S+")
       printfn "Scanning %s, found %d links" url matches.Count 
    }

// Async calls made here

let parseUrls() =
    Async.RunSynchronously(
        Async.Parallel [for i in 0..urls.Length-1 -> downloadUrl urls.[i]])
        
let parseUrlsPipe() = 
   [for i in 0 .. urls.Length - 1 -> downloadUrl urls.[i]]
        |> Async.Parallel
        |> Async.RunSynchronously
                

// let!, do! and return! 

// let binds a value or function to an identifier - nothing new here. 
// In contrast, let! executes an async workflow on its own thread 
// and binds its return value to an identifier. 
// You can simply treat the async operation as the value it returns. 
// Note that when a workflow started by let! returns, 
// the rest of the original workflow will continue to run on the new thread. 
// If you use let!  and watch the thread ID of an async workflow over time, 
// you will see the workflow run on different threads as async child workflows complete
//  and return.

// do executes expressions synchronously, while do! executes expressions asynchronously. 
// do! is used to execute an expression whose return value is uninteresting 
// and can be ignored.

// return returns a result, while return! executes an async workflow and 
// returns its return value as a result. 


// Cancelling Async Workflows

// Async workflow that takes a long time.

let longOperationAsync delay =
    async {
        printfn "doing time-consuming thing..."
        printfn "waiting %d seconds" delay
        let wait = delay * 1000
        Async.Sleep(delay) |> ignore
        printfn "time-consuming operation complete"
    }

// Cancellation function.
// Called if the async workflow is cancelled.
let onCancelled (c: OperationCanceledException) =
    printfn "operation canceled: %s" c.Message
// Sets up async workflow that can be cancelled.
// Associates async workflow with cancellation function.
let delay = 15
Async.TryCancelled(longOperationAsync delay, onCancelled)
    |> Async.Start
   

if delay > 10 then  // >10 secs is too long for us to wait!
    Async.CancelDefaultToken()