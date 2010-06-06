#light

module Sample.ObjectOriented

open System   // provides access to DateTime type

type Movie(title: string, released: DateTime) =

    let mutable _releaseDate = released

    member this.ReleaseDate

       with get() =

          _releaseDate

       and set(newValue) =

          _releaseDate <- newValue

