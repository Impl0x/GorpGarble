/// Implementation of a solution to the closest-pair problem.
module ClosestPair

open System

open Geometry

let dist = Point.Dist

/// Gets the closest pair using a brute-force O(n^2) approach.
let closestBf points =
     let n = List.length points
     seq { 
        for i in 0..n-2 do
            for j in i+1..n-1 do
                yield points.[i], points.[j] }
     |> Seq.minBy (fun (a, b) -> dist a b)
 

/// Gets the closest pair using a divide-and-conquer O(n log n) approach.
let rec closestInternal points =
    let length = Seq.length points
    match length with
    | x when x < 4 -> closestBf points
    | _ -> 
        //partition points about a vertical line
        let sorted = points |> List.sortBy (fun p -> p.X)
        let left, right = points |> List.splitAt (length / 2)
 
        //recurse each side of the vertical line
        let lMin = closestInternal left
        let rMin = closestInternal right
 
        //find minimum distance between closest pairs on each side of the line
        let lDist =
            match lMin with
            | (a, b) -> dist a b
 
        let rDist = 
            match rMin with
            | (a, b) -> dist a b
  
        let minDist = Math.Min (lDist, rDist)
        let dividingX = left |> Seq.toList |> List.rev |> List.head |> (fun p -> p.X)
 
        //find close points on the right to the dividing line
        let closePoints = 
            right 
            |> Seq.takeWhile (fun p -> p.X - dividingX < minDist) 
            |> Seq.sortBy (fun p -> p.Y)
  
        //take the close points and merge them with the close points to the dividing line on the left hand side
        let pairs = 
            left 
            |> Seq.skipWhile (fun p -> dividingX - p.X > minDist) 
            |> Seq.collect (fun p -> 
                closePoints 
                |> Seq.skipWhile (fun p1 -> p1.Y < p.Y - minDist) 
                |> Seq.takeWhile (fun p2 -> p2.Y < p.Y + minDist) 
                |> Seq.map (fun a -> (p, a)))
            |> Seq.toList
 
        //return the closest pair of points from the three groups
        pairs @ [lMin; rMin] |> List.minBy (fun (a, b) -> dist a b)

let closestPair = closestInternal
