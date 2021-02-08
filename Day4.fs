module Day4

open System.IO
open FSharp.Control

module AsyncSeq =

    let fileLines (filename: string) : AsyncSeq<string> =
        asyncSeq {        
            use reader = new StreamReader(filename) :> TextReader
            let rec go () = asyncSeq {
                let! line = reader.ReadLineAsync() |> Async.AwaitTask
                if line <> null then
                    yield line
                    yield! go ()
            }
            yield! go ()
        }

    /// Splits a sequence in to multiple sequences, using the given predicate to determine the
    /// boundary of each chunk. Elements matching the predicate are discarded.
    let splitBy (p: 'a -> bool) (input: AsyncSeq<'a>) : AsyncSeq<AsyncSeq<'a>> =
        asyncSeq {
            let enumerator = input.GetEnumerator()

            let rec restOfChunk () : AsyncSeq<'a> =
                asyncSeq {
                    let! nextValue = enumerator.MoveNext()
                    match nextValue with
                    | Some value when not (p value) ->
                        yield value
                        yield! restOfChunk ()
                    | _ -> return ()
                }

            let rec nextChunk () : Async<AsyncSeq<'a> option> =
                async {
                    let! nextValue = enumerator.MoveNext()
                    match nextValue with
                    | Some value when p value ->
                        return! nextChunk()
                    | Some value ->
                        let chunk = AsyncSeq.append (AsyncSeq.singleton value) (restOfChunk ())
                        return Some(chunk)
                    | _ ->
                        return None
                }
        
            yield!
                AsyncSeq.unfoldAsync
                    (fun () -> async {
                        let! chunk = nextChunk ()
                        return Option.map (fun x -> x, ()) chunk
                    }) ()
        }

let parseLine (line: string) : (string * string) list =
    line.Split(' ')
    |> Seq.map
        (fun str ->
            match str.Split(':') |> Seq.toList with
            | [k; v] -> k, v
            | _ -> failwithf "invalid token: %s" str)
    |> Seq.toList

let requiredKeys =
    [
        "byr"
        "iyr"
        "eyr"
        "hgt"
        "hcl"
        "ecl"
        "pid"
    ]

let isValid (passport: Map<string, string>) : bool =
    requiredKeys |> Seq.forall (fun key -> Map.containsKey key passport)

let runDay4 () =

    let (valid, invalid) =
        AsyncSeq.fileLines "..\\..\\..\\resources\\passports.txt"
            |> AsyncSeq.splitBy (System.String.IsNullOrEmpty)
            |> AsyncSeq.foldAsync (fun (valid, invalid) chunk -> async {
                    let! lines = AsyncSeq.toListAsync chunk
                    let passport = lines |> Seq.collect parseLine |> Map.ofSeq
                    if isValid passport then
                        Seq.iter (printfn "%s") lines
                        printfn "\n%s\n" (String.replicate 80 "*")
                        return (valid + 1, invalid)
                    else
                        return (valid, invalid + 1)
                }) (0, 0)
            |> Async.RunSynchronously

    printfn "%i are valid, %i are invalid" valid invalid
