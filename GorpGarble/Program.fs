module Program
open System
open System.Drawing
open System.Windows.Forms
open System.Drawing.Imaging
open Microsoft.FSharp.NativeInterop

open Util
open Geometry

type Window () as this =
    inherit Form ()
    let mutable time = 0.0
    
    let sample = 
        new Bitmap (System.Reflection.Assembly.GetCallingAssembly().GetManifestResourceStream "greep-big.png")

    do
        this.Text <- "GorpGarble"
        this.Width <- 640
        this.Height <- 480
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)

    override this.OnPaint e =
        time <- time + 0.1
        let clock = abs (sin time * 3.0)
        let clock2 = cos (time / 15.0) * 15.0

        let input = sample
        let inputData = input.LockBits (Drawing.Rectangle (0, 0, input.Width, input.Height), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)
        let inputPtr = NativePtr.ofNativeInt inputData.Scan0

        let output = new Bitmap (this.Width, this.Height)
        let outputData = output.LockBits (Drawing.Rectangle (0, 0, output.Width, output.Height), ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb)
        let outputPtr = NativePtr.ofNativeInt outputData.Scan0

        let triDeform (a : Vector) (aB : Vector) (aC : Vector) (b : Vector) (bC : Vector) (bA : Vector) (c : Vector) (cA : Vector) (cB : Vector) (input : Vector) =
            let u, v = input.X, input.Y
            let aT = 1.0 - u - v
            let bT = u
            let cT = v
            aT * aT * (aT * a + bT * (a + aB / 3.0) + cT * (a + aC / 3.0)) +
            bT * bT * (bT * b + cT * (b + bC / 3.0) + aT * (b + bA / 3.0)) +
            cT * cT * (cT * c + aT * (c + cA / 3.0) + bT * (c + cB / 3.0))

        let v x y = Vector (x, y)
        let deform = triDeform (v 0.0 0.0) (v clock 0.3) (v 0.3 (0.5 * -clock)) (v (0.5 * clock) (0.01 * clock)) (v clock 0.1) (v 0.5 -1.0) (v 0.0 1.0) (v 0.3 1.0) (v 1.0 -1.0)

        let resolution = 2000
        let delta = 1.0 / float resolution
        for i = 0 to resolution - 1 do
            for j = 0 to resolution - 1 do
                let input = Vector ((float i + 0.5) * delta, (float j + 0.5) * delta)
                let iX = int (float input.X * float inputData.Width - 0.5)
                let iY = int (float input.Y * float inputData.Height - 0.5)
                let iX = (iX % inputData.Width + inputData.Width) % inputData.Width
                let iY = (iY % inputData.Height + inputData.Height) % inputData.Height
                let color = NativePtr.get inputPtr (iX + iY * inputData.Width) : int
                let output = deform input
                let oX = int (float output.X * float outputData.Width - 0.5)
                let oY = int (float output.Y * float outputData.Height - 0.5)
                if oX >= 0 && oY >= 0 && oX < outputData.Width && oY < outputData.Height then
                    NativePtr.set outputPtr (oX + oY * outputData.Width) color

        input.UnlockBits inputData
        output.UnlockBits outputData

        e.Graphics.DrawImageUnscaled (output, 0, 0)
        this.Invalidate ()

[<EntryPoint; STAThread>]
let main args =
    Application.EnableVisualStyles ()
    let window = new Window ()
    Application.Run window
    0