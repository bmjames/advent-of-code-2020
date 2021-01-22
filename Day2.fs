module Day2

open System.IO
open FParsec

type Policy = { Min: int; Max: int; Letter: char }

type Password = Password of string

let parseLine<'a> : Parser<Policy * Password, 'a> =
    pint32 .>>. (pchar '-' >>. pint32) >>= fun (min, max) -> 
    spaces1 >>. (anyChar .>> pchar ':') >>= fun letter ->
    spaces1 >>. restOfLine false |>> fun password ->
    { Min = min; Max = max; Letter = letter; }, Password password

let isValid (policy: Policy) (password: string): bool =
    let occurs = password |> Seq.filter ((=) policy.Letter) |> Seq.length
    occurs >= policy.Min && occurs <= policy.Max

let runDay2 () =

    let inputs =
        File.ReadLines "..\\..\\..\\resources\\passwords.txt"
        |> Seq.map (run parseLine)
        |> Seq.map (function | Success(result, _, _) -> result
                             | Failure(errMsg, _, _) -> failwith errMsg)
        |> Seq.toList

    inputs
        |> Seq.iter
            (fun (policy, Password pw) ->
                let assessment = if isValid policy pw then "  valid:" else "invalid:"
                printfn "%s %i-%i %c %s" assessment policy.Min policy.Max policy.Letter pw
            )

    let validPasswords = inputs |> Seq.filter (fun (policy, Password pw) -> isValid policy pw)
    printfn "\nThere are %i valid passwords." (Seq.length validPasswords)
