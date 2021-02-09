module Day4

open System
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
        
            yield! AsyncSeq.replicateUntilNoneAsync (nextChunk())
        }

let parseLine (line: string) : (string * string) list =
    line.Split(' ')
    |> Seq.map
        (fun str ->
            match str.Split(':') |> Seq.toList with
            | [k; v] -> k, v
            | _ -> failwithf "invalid token: %s" str)
    |> Seq.toList

let tryParseWith (f: string -> bool * 'a) : string -> 'a option =
    f >>
    function
    | true, x -> Some x
    | false, _ -> None

let tryParseInt = tryParseWith Int32.TryParse

let inRange lowerBound upperBound x = x >= lowerBound && x <= upperBound

/// Combine 2 predicates with AND
let (&&&) f g x = f x && g x

// Combine 2 predcates with OR
let (|||) f g x = f x || g x

let validateHeight (input: string) =
    let digits = input |> Seq.takeWhile Char.IsDigit |> String.Concat
    let unit = input |> Seq.skipWhile Char.IsDigit |> String.Concat
    match unit with
    | "cm" -> tryParseInt digits |> Option.exists (inRange 150 193)
    | "in" -> tryParseInt digits |> Option.exists (inRange 59 76)
    | _ -> false

let validateHairColour (input: string) =
    match input.ToCharArray() |> List.ofArray with
    | '#' :: tail -> List.length tail = 6 && List.forall (Char.IsDigit ||| inRange 'a' 'f') tail
    | _ -> false

let validateEyeColour input = List.contains input ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

let validatePid (input: string) = String.length input = 9 && String.forall Char.IsDigit input

let isValid : Map<string, string> -> bool =

    (Map.tryFind "byr" >> Option.bind tryParseInt >> Option.exists (inRange 1920 2002))
    &&& (Map.tryFind "iyr" >> Option.bind tryParseInt >> Option.exists (inRange 2010 2020))
    &&& (Map.tryFind "eyr" >> Option.bind tryParseInt >> Option.exists (inRange 2020 2030))
    &&& (Map.tryFind "hgt" >> Option.exists validateHeight)
    &&& (Map.tryFind "hcl" >> Option.exists validateHairColour)
    &&& (Map.tryFind "ecl" >> Option.exists validateEyeColour)
    &&& (Map.tryFind "pid" >> Option.exists validatePid)

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
