module Tests

open System
open FsCheck
open global.Xunit

/// Generates a sequence which is at least 2 elements long, with one missing number which is
/// neither the minimum nor the maximum
let genSeq (x: uint) (y: uint) (z: uint) : Gen<uint array> * uint =
    let seqStart = Math.Min(x, y) + 1u
    let seqEnd = Math.Max(x, y) + 3u
    let removed = seqStart + 1u + (z % (seqEnd - seqStart - 1u))
    let xs = [ seqStart .. seqEnd ] |> Set.ofList |> Set.remove removed
    Gen.shuffle xs, removed

[<Fact>]
let ``findMissing returns the missing element from an otherwise contiguous set of integers`` () =
    let go (x: uint, y: uint, z: uint) =
        let listGen, missing = genSeq x y z
        listGen |> Gen.map (fun xs -> Day5.findMissing xs = missing)

    Check.QuickThrowOnFailure go
