/// Contains functions relevant to finding the circumcircle of three points.
module Circumcircle

open System

open Geometry

/// The length of a side of a triangle.
type SideLength = float

/// Given three points, returns the lengths of each side of a triangle with the given verticies
let inline getLengths (a : Point) (b : Point) (c : Point) =
    let ab = Point.Dist a b
    let bc = Point.Dist b c
    let ac = Point.Dist c a
    (ab, bc, ac)

/// Returns the radius of the circumcircle for a triangle with the given verticies.
let radius (a : Point, b : Point, c : Point) =
    let ab, bc, ac = getLengths a b c
    let num = ab * bc * ac
    let denom = sqrt ((ab + bc + ac) * (bc + ac - ab) * (ac + ab - bc) * (ab + bc - ac))
    num / denom

/// Computes the intersection of two lines.
let intersection (point1 : Point, dir1 : Vector) (point2 : Point, dir2 : Vector) =
    (*if point1 = point2 then point1 
    else
        let x1, y1 = point1 |> (fun p -> (p.X, p.Y))
        let x2, y2 = point2 |> (fun p -> (p.X, p.Y))
        let x3, y3 = (point1 + dir1) |> (fun p -> (p.X, p.Y))
        let x4, y4 = (point2 + dir2) |> (fun p -> (p.X, p.Y))
        let numX = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * x4 - y3 * x4)
        let numY = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * x4 - y3 * x4)
        let denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        Point (numX / denom, numY / denom)*)

/// Returns the circumcenter of a triangle of the given verticies
let circumcenter (a : Point, b : Point, c : Point) =
    let max3 a b c =
        let inline max a b = if a > b then a else b
        max a (max b c)

    /// Returns the midpoint of a side on a triangle, where the side is defined as a vertex point and a direction vector.
    let midpoint (aPos : Point) (aDir : Vector) : Point = aPos + (0.5 * aDir)

    /// Finds the inside-oriented perpendicular bisector of a given side on a triangle.
    let innerBisector (side : Vector) (reference : Vector) =
        let normA, normB = Vector.Norm side
        if normA * reference >= 0.0 then normA else normB

    let ab = b - a
    let bc = c - b
    let ac = c - a

    match (max3 ab.Length bc.Length ac.Length) with
    | n when n = ab.Length -> (c, bc, ac)
    | n when n = bc.Length -> (a, ab, ac)
    | n when n = ac.Length -> (b, ab, bc)
    |> (fun (p, v1, v2) ->
            let midV1 = midpoint p v1
            let midV2 = midpoint p v2
            let v1Norm = innerBisector v1 v2
            let v2Norm = innerBisector v2 v1
            intersection (midV1, v1Norm) (midV2, v2Norm))
