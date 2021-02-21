module Day9

open System
open System.IO

let combinations (xs: 'a seq) : ('a * 'a) seq =
    xs |> Seq.collect (fun x -> Seq.map (fun y -> x, y) xs)

let isValid window : bool =
    combinations window
    |> Seq.filter (fun (x, y) -> x <> y)
    |> Seq.map (fun (x, y) -> x + y)
    |> Seq.exists (fun x -> x = Array.last window)

let getInput () =
    File.ReadLines @"..\..\..\resources\cipher.txt"
    |> Seq.map Int64.Parse

let rec tails xs =
    if Seq.isEmpty xs then Seq.empty
    else Seq.append (Seq.singleton xs) (tails (Seq.tail xs))

let findSum target (xs : int64 array) : seq<int64> option =
     Seq.scan (+) 0L xs
     |> Seq.tryFindIndex ((=) target)
     |> Option.map (fun i -> Seq.take (i + 1) xs)

let run () =

    // Part 1

    let invalid =
        getInput ()
        |> Seq.windowed 26
        |> Seq.find (Seq.toArray >> isValid >> not)
        |> Array.last
    
    printfn "First invalid number: %i" invalid

    // Part 2
    
    let addends =
        getInput ()
        |> tails
        |> Seq.pick (Seq.toArray >> findSum invalid)

    let min, max = Seq.min addends, Seq.max addends

    printfn "\nThese numbers sum to %i: %A" invalid addends
    printfn "The answer is %i + %i = %i" min max (min + max)
