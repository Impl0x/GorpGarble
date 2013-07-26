module Geometry

/// Represents a vector in 2D space.
type [<Struct>] Vector (x : float, y : float) =
    /// The X component of this vector.
    member this.X = x
    /// The Y component of this vector.
    member this.Y = y
    /// The squared magnitude of this vector.
    member this.SqLength = x * x + y * y
    /// The magnitude of this vector.
    member this.Length = sqrt this.SqLength

    /// Returns the result of multiplying a vector by a scalar.
    static member (*) (A : Vector, k : float) = Vector (A.X * k, A.Y * k)
    static member (*) (k : float, A : Vector) = Vector (A.X * k, A.Y * k)
    static member (/) (A : Vector, k : float) = Vector (A.X / k, A.Y / k)
    /// Returns the dot product of two vectors.
    static member (*) (A : Vector, B : Vector) = A.X * B.X + A.Y * B.Y
    /// Returns the result of adding two vectors.
    static member (+) (A : Vector, B : Vector) = Vector (A.X + B.X, A.Y + B.Y)
    /// Returns the result of subtracting one vector by another.
    static member (-) (A : Vector, B : Vector) = A + (B * - 1.0)
    /// Returns the distance between two vectors.
    static member Dist (A : Vector) (B : Vector) = (A - B).Length
    /// Returns the two vector normals to a given vector.
    static member Norm (A : Vector) = (Vector (A.Y, -A.X), Vector (-A.Y, A.X))

    override this.ToString () =  "(" + string this.X + ", " + string this.Y + ")"

/// Represents a point in 2D space.
type Point = Vector