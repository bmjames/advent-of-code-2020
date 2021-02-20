module Day7

open System.IO
open FParsec

/// e.g. BagRule("light red", [1, "bright white"; 2, "muted yellow"])
type BagRule = string * (int * string) list

// e.g. light red bags contain 1 bright white bag, 2 muted yellow bags.
let parseBagRule<'a> : Parser<BagRule, 'a> =
    let anyStringTill = manyCharsTill anyChar
    let parseContents = pint32 .>>. (pchar ' ' >>. anyStringTill (pstring " bags" <|> pstring " bag"))

    anyStringTill (pstring " bags contain ") >>= fun container ->
    ((stringReturn "no other bags" []) <|> (sepBy parseContents (pstring ", "))) .>> pchar '.' |>> fun contents ->
    BagRule(container, contents)

let run () =

    let rules =
        File.ReadLines @"..\..\..\resources\bags.txt"
        |> Seq.map (CharParsers.run parseBagRule)
        |> Seq.map (function | Success(result, _, _) -> result
                             | Failure(errMsg, _, _) -> failwith errMsg)

    // Part 1

    let bagsByContainer =
        rules
        |> Seq.collect (fun (container, contents) -> contents |> Seq.map (fun (_, colour) -> colour, container))
        |> Seq.groupBy fst
        |> Seq.map (fun (contained, containers) -> contained, Seq.map snd containers)
        |> Map.ofSeq

    let rec containedBy bag =
        let containers = Map.tryFind bag bagsByContainer |> Option.defaultValue Seq.empty
        Seq.append containers (Seq.collect containedBy containers)

    let answer = containedBy "shiny gold" |> Set.ofSeq
    printfn "Shiny gold bags can be contained by %i bags: %A" (Set.count answer) (List.ofSeq answer)
