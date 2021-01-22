module Day2

open System.IO
open FParsec

type Policy = { Pos1: int; Pos2: int; Letter: char }

type Password = Password of string

let parseLine<'a> : Parser<Policy * Password, 'a> =
    pint32 .>>. (pchar '-' >>. pint32) >>= fun (pos1, pos2) -> 
    spaces1 >>. (anyChar .>> pchar ':') >>= fun letter ->
    spaces1 >>. restOfLine false |>> fun password ->
    { Pos1 = pos1; Pos2 = pos2; Letter = letter; }, Password password

let isValid (policy: Policy) (password: string): bool =
    let chars = Seq.toArray password
    (chars.[policy.Pos1 - 1] = policy.Letter) <> (chars.[policy.Pos2 - 1] = policy.Letter)

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
                printfn "%s %i-%i %c: %s" assessment policy.Pos1 policy.Pos2 policy.Letter pw
            )

    let validPasswords = inputs |> Seq.filter (fun (policy, Password pw) -> isValid policy pw)
    printfn "\nThere are %i valid passwords." (Seq.length validPasswords)
