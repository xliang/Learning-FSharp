#light

module Sample.UnitOfMeasure

[<Measure>] type usd
[<Measure>] type eu

let salary = 100000<usd>
let raise = salary + 1000<usd>


