#light

module Objectville.FSharp.Sample.Collection

//------------------Array---------------------//

// Create an array 0..9 using a lambda function to initalize the elements
let nums = Array.init 10 (fun n -> n)

//------------------List----------------------// 

let places = 
    [("Seatle", 594210); ("Prague", 1188126); ("New York", 7180000);
    ("Grantchester", 552); ("Cambridge", 117900)]

//-----------------Sequence--------------------//


