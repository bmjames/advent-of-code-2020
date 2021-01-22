module Day3

open System.IO

let parseTrees (line: string) : bool array =
    line
    |> Seq.map (function | '.' -> false; | '#' -> true; | _ -> failwith "parse error")
    |> Seq.toArray

let runDay3 () =

    let trees =
        File.ReadLines "..\\..\\..\\resources\\trees.txt"
        |> Seq.map parseTrees
        |> Seq.toArray

    let width = Array.head trees |> Array.length

    // Part 1
    // x-index (position on the row) is equal to 3 * y-index, modulo width
    let treeCount =
        trees
        |> Seq.indexed
        |> Seq.filter (fun (i, row) -> row.[(3 * i) % width])
        |> Seq.length

    printfn "%i trees were encountered." treeCount
