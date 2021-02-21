module Day8

open System
open System.IO
open FSharp.Text.RegexProvider

type Instr = Nop of int | Acc of int | Jmp of int

type InstrRegex = Regex< @"(?<Instr>nop|acc|jmp) (?<Arg>[\+\-]\d+)$" >

type EvalResult = Diverge of int | Terminate of int | InvalidAddrError

module EvalResult =
    let tryTerminate =
        function
        | Diverge _ -> None
        | Terminate x -> Some x
        | InvalidAddrError -> None

let parseInstr line input =
    let rmatch = InstrRegex().TypedMatch(input)
    let arg = Int32.Parse rmatch.Arg.Value
    match rmatch.Instr.Value with
    | "nop" -> Nop arg
    | "acc" -> Acc arg
    | "jmp" -> Jmp arg
    | x -> failwithf "Line %i: Invalid instruction '%s' (%s)" line x input

let step (acc: int) (addr: int) (instr: Instr) : int * int = // acc * next addr
    match instr with
    | Nop _ -> acc, addr + 1
    | Acc x -> acc + x, addr + 1
    | Jmp x -> acc, addr + x

let eval (program: Instr array) : EvalResult =
    let rec go (acc: int) (addr: int) (visited: int Set) =
        if addr = Array.length program then
            Terminate acc
        elif Seq.contains addr visited then
            Diverge acc
        elif addr < 0 || addr > Array.length program then
            InvalidAddrError
        else
            let newAcc, nextAddr = step acc addr program.[addr]
            go newAcc nextAddr (Set.add addr visited)

    go 0 0 Set.empty

let searchPrograms (program: Instr array) : seq<Instr array> =
    let modifyProgram addr newInstr =
        let modified = Array.copy program
        modified.[addr] <- newInstr
        modified

    let rec go (addr: int) =
        match Array.tryItem addr program with
        | None -> Seq.empty
        | Some instr ->
            let modifiedProgram =
                match instr with
                | Acc _ -> []
                | Nop arg -> [modifyProgram addr (Jmp arg)]
                | Jmp arg -> [modifyProgram addr (Nop arg)]
            Seq.append modifiedProgram (go (addr + 1))

    go 0

let run() =

    let program =
        File.ReadLines @"..\..\..\resources\halting.txt"
        |> Seq.mapi parseInstr
        |> Seq.toArray

    // Part 1
    printfn "Value in accumulator: %A" (eval program)

    // Part 2
    searchPrograms program
    |> Seq.tryPick (eval >> EvalResult.tryTerminate)
    |> Option.iter (printfn "Accumulator upon termination: %i")
