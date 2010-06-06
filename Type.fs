#light

module Sample.Type

// Enum
type Color =
    | Red = 0 | Green = 1 | Blue = 2

let myColor = Color.Blue

// Record

type Movie = {
     Title : string;
     ReleasYear : int;
}    

// Reference Call

let daughter = ref "Helen"

daughter := "Helena"

// Access a reference cell with operator (!)
printfn "daughter name is %s" !daughter

// Generic Types

type 'a Stack =
    | EmptyStack
    | StackNode of 'a * 'a Stack
    
let iStack = StackNode(3, StackNode(5, EmptyStack))
let fStack = StackNode(2.0, StackNode(1.0, EmptyStack))
let sStack = StackNode("world", StackNode("hello", EmptyStack))             

// Tuple
let withItem2 newItem2 (originalItem1, _) = (originalItem1, newItem2)


// called complete pattern in matching 
let withItem2AnotherSample newItem2 tuple = 
    match tuple with
    | (originalItem1, _) -> (originalItem1, newItem2)


