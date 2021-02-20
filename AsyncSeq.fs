module AsyncSeq

open System.IO
open FSharp.Control

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
