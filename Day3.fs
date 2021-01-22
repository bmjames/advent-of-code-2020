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

    // x-index (position on the row) is equal to 3 * y-index, modulo width
    let countTrees xVelocity yVelocity =
        trees
        |> Seq.indexed
        |> Seq.filter (fun (i, _) -> i % yVelocity = 0)
        |> Seq.filter (fun (i, row) -> row.[(xVelocity * (i / yVelocity)) % width])
        |> Seq.length

    let scenarios = [1,1; 3,1; 5,1; 7,1; 1,2]
    
    let product =
        scenarios
        |> List.fold
            (fun state (xVelocity, yVelocity) ->
                let trees = countTrees xVelocity yVelocity
                printfn "Right %i, down %i: %i trees" xVelocity yVelocity trees
                state * trees)
            1

    printfn "\nProduct: %i" product
