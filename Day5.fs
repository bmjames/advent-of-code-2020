module Day5

open System
open System.IO

let readIds () =
    File.ReadLines @"..\..\..\resources\boarding.txt"
    |> Seq.map (fun line -> line.Replace('F', '0').Replace('B', '1').Replace('L', '0').Replace('R', '1'))
    |> Seq.map (fun line -> Convert.ToUInt32(line, 2))

let findMissing (xs: uint seq): uint =

    let min, max, sum =
        xs |> Seq.fold (fun (min, max, acc) x -> Math.Min(x, min), Math.Max(x, max), x + acc) (UInt32.MaxValue, UInt32.MinValue, 0u)

    let expectedSum = (max * (max + 1u) - min * (min - 1u)) / 2u // From the sum of first n natural numbers
    
    expectedSum - sum

let run () =

    // Part 1
    readIds () |> Seq.max |> printfn "Highest seat id: %i"

    // Part 2
    readIds () |> findMissing |> printfn "Missing id: %i"
