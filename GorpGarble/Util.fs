[<AutoOpen>]
module Util

open System

type Random () =
    inherit System.Random ()

    ///Returns a new float in the range [0 .. n).
    member this.NextDouble n = (float (this.Next (n * 10))) / 10.0 + this.NextDouble ()

/// Contains useful extension functions for the F# List module.
module List =
    /// Returns the first n elements in a list.
    let inline take n (coll : _ list) = [for i = 0 to n - 1 do yield coll.[i]]

    /// Drops the first n elements in a list and returns what remains.
    let inline drop n (coll : _ list) = [for i = n to coll.Length - 1 do yield coll.[i]]

    /// Takes a given collection and returns two collections that are partitioned at the given index.
    let inline splitAt n (coll : _ list) = (take n coll, drop n coll)

let rand = new Random ()