module Day6

open FSharp.Control

let run () =

    let sumWith f =
        AsyncSeq.fileLines @"..\..\..\resources\customs.txt"
        |> AsyncSeq.splitBy (System.String.IsNullOrEmpty)
        |> AsyncSeq.mapAsync (AsyncSeq.map Set.ofSeq >> AsyncSeq.toListAsync)
        |> AsyncSeq.map (f >> Set.count)
        |> AsyncSeq.sum
        |> Async.RunSynchronously

    printfn "Part 1: %i" (sumWith Set.unionMany)
    printfn "Part 2: %i" (sumWith Set.intersectMany)
