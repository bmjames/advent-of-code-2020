module Day1

open System.IO

let run () =

    let dates =
        File.ReadLines "..\\..\\..\\resources\\dates.txt"
        |> Seq.map int
        |> Seq.toArray

    let lookup = Array.create 2020 false
    dates |> Seq.iter (fun date -> lookup.[date] <- true)

    let findPairWithSum sum =
        dates
        |> Seq.filter (fun x -> x < sum)
        |> Seq.choose (fun x -> let y = sum - x in if lookup.[y] then Some((x, y)) else None)
        |> Seq.tryHead

    printfn "Part 1:"
    findPairWithSum 2020 |> Option.iter (fun (x, y) -> printfn "%i * %i = %i" x y (x * y))

    printfn "\nPart 2:"
    dates
    |> Seq.map (fun x -> 2020 - x)
    |> Seq.choose findPairWithSum
    |> Seq.tryHead
    |> Option.iter (fun (y, z) -> let x = 2020 - y - z in printfn "%i * %i * %i = %i" x y z (x * y * z))
