#light

module Objectville.FSharp.Sample.IO

open System
open System.IO
open System.Security.Cryptography
open System.Diagnostics

// List all files in given directory and all the subdirectories
//
// Surelly, there might be some files you are not given access to
// So, ignore all the exceptions
let rec allFiles dir =
    Seq.append
        (try
            // return the list of files in the given directory
            dir |> Directory.GetFiles           
         with e -> [||])
        (try
            // dive into subfolders
            dir |> Directory.GetDirectories |> Seq.map allFiles |> Seq.concat
         with e -> Seq.empty)


// As we will work with I/O, that operations can take lots of time
// Unfortunately, at this moment you cannot tell just by looking at F# Interactive window whether it is working or not
//
// As F# sequences are lazy, we can monitor when their elements are accessed by providing a 'proxy' sequence
//
// Visualize function takes a report function and a sequence to watch
// it calls the report function periodically (for not to bother you too often)
//
// report function has 3 arguments: current index of the sequence, current element, time span since start of enumeration
//
// tsReportInterval is minimal interval between subsequent calls of report function
let tsReportInterval = TimeSpan.FromMilliseconds(500.0)
//
let Visualize reportfunc sq =
    seq {
        let start = DateTime.Now                            // moment of start
        let dt = ref DateTime.Now                           // moment of start or last reporting
        let num = ref 0                                     // index of the current element
        for i in sq ->
            let cur = DateTime.Now
            if (cur.Subtract(!dt) > tsReportInterval) then  // if it's time to report
                reportfunc !num i (cur.Subtract(start))     
                dt := cur                                   // update time of last reporting
            num := !num + 1
            i                                               // yield element
    }


// specify folders for consideration
// surely, yours will be different

//let folders = [@"C:\Share\"; @"d:\"; @"h:\"; @"v:\"; @"w:\"; @"x:\"; @"y:\"; @"z:\"];

let folders = [@"H:\dev\z.books"];

// list of all the files to consider
let files =
    folders
        |> List.map allFiles    // map each path to sequence files in that hive
        |> List.map (Visualize (fun _ i ts -> printfn "%A %A" ts i)) // put displaying proxy in between
        |> Seq.concat           // concatenate that sequences into one
        |> Seq.toArray          // now transform that lazy sequence into the final array of files


// The hashing algorithm class:
let sha = new SHA1CryptoServiceProvider()
// Number of bytes to consider
let maxLength = 10000
// buffer for reading
let (buf : byte[]) = Array.create maxLength 0uy

// retrieves a signature for a file
//
// return value is (exists : bool, length : ulong, signature : byte[])
let fileSign file = 
    let mutable res = (false, 0L, null) // by default, it doesn't exist
    try
        use fs = File.OpenRead(file)
        let flLen = fs.Length
        let len = int (min (int64 maxLength) flLen)

        // read 'len' bytes        
        let mutable pos = 0
        while (pos < len) do
            let chunk = fs.Read(buf, pos, len - pos)
            pos <- pos + chunk

        // get signature            
        let sign = sha.ComputeHash(buf, 0, len)
        
        // store new result
        res <- (true, flLen, sign)        
    with e -> e |> ignore
    res                                 // return

// how many files are there ?
let total = files.Length

// gets percentage in float
let get_perc cur total =
    100.0 * float cur / float total
    
// data for all the files
let data =
    files
        |> Visualize (fun x i ts -> printfn "%6.2f %A %A" (get_perc x total) ts i)
        |> Seq.map (fun f -> (f, fileSign f))
        |> Seq.toArray
// now for each file we have (name, (exist, length, signature)) in an array


// let's value our time: small duplicates are not worth our attention
//
// Therefore, let's invent a threshold on size:
let considerableLength = int64 (2 * 1024 * 1024) // 2 Mib
// (Set to 0 if you do not want such)

// Let's make the data presentable:
// filter our unexisting and small,
//
// represent it as (key, value) pairs,
// to be more specificic, of ((len, signature), name)
// because (len, signature) tuple represent file's uniqueness
let characteristics = 
    data
        |> Seq.filter (fun (name, (exist, len, sign)) -> exist)
        |> Seq.filter (fun (name, (exist, len, sign)) -> len >= considerableLength)
        |> Seq.map (fun (name, (exist, len, sign)) -> ((len, sign), name))
        |> Seq.toArray

// Let's build an array of keys
let keys =
    characteristics
        |> Seq.map (fun (k, v) -> k)
        |> Set.ofSeq
        |> Seq.toArray

// here is a funcation that gives array of names for the given key
let names key =
    characteristics
        |> Seq.filter (fun (k, v) -> k = key)
        |> Seq.map (fun (prop, name) -> name)
        |> Seq.toArray

// total number of keys
let keyLength = keys.Length

// duplicates
let dups = 
    keys
        |> Visualize (fun x _ _ -> printfn "%6.2f" (get_perc x keyLength))
        |> Seq.map (fun (len, sign) -> (len, names (len, sign)))
        |> Seq.filter (fun (len, names) -> names.Length > 1)
        |> Seq.toArray

// Sort dups by size descending
dups
    |> Array.sortInPlaceWith (fun (len1, _) (len2, _) -> Math.Sign(int64 (len2 - len1)))
    
// path to desktop
let desktop = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)
// path to report
let report = Path.Combine(desktop, DateTime.Now.ToString("D") + " - duplicates.txt");

// write dups to files
let res = new StreamWriter(report)
for el in dups do
    let (size, names) = el
    fprintfn res "size = %A" size
    for n in names do
        fprintfn res "%s" n
    fprintfn res "-----------------------------------------"
res.Dispose()


