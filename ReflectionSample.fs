#light



// Reflection By Value



module Sample.Reflection

open Microsoft.FSharp.Reflection

// TODO: need to fix

//    let printTupleValue (x : obj) = 
//        if FSharpType.IsTuple(x.GetType()) then
//            let vals = FSharpValue.GetTupleFields x 
//                printf "("
//                vals 
//                |> Seq.iteri
//                (fun i v -> 
//                    if i <> Seq.length vals - 1 then
//                        printf " %A, " v
//                    else
//                      printf " %A" v)
//          printfn " )"
//       else
//          printfun "not a tuple"