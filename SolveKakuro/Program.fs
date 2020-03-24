// Learn more about F# at http://fsharp.org

open System
open System.IO

type Equation = int * int list

let minTotalPossible n = n * (n+1) / 2
let maxTotalPossible n = n * (19 - n) / 2

let rec isValidSolutionSoFar (eqns: Equation list) (solution: int option list) : bool =
    match eqns with
    | [] -> true
    | (targetSum, vars)::tail ->
        let vals = vars |> List.map(fun i -> solution.[i])
        let actualSum = vals |> List.map(Option.defaultValue 0) |> List.sum
    
        let noRepeats = (vals |> Seq.where(Option.isSome) |> Seq.distinct |> Seq.length) = (vals |> Seq.where(Option.isSome) |> Seq.length)
        let numEmpty = vals |> Seq.where(Option.isNone) |> Seq.length
    
        noRepeats
        && actualSum <= targetSum - (minTotalPossible numEmpty)
        && actualSum >= targetSum - (maxTotalPossible numEmpty)
        && isValidSolutionSoFar tail solution

[<EntryPoint>]
let main argv =
    let board = File.ReadLines("../../../Board1.txt")
    let readBoard r c = board |> Seq.tryItem(r) |> Option.map(Seq.tryItem(c)) |> Option.flatten

    let acrossNums = (board |> Seq.item(10)).Split(' ') |> Seq.map(Int32.Parse)
    let downNums = (board |> Seq.item(11)).Split(' ') |> Seq.map(Int32.Parse)

    let vars: (int*int) list = [
        let acrossEnum = acrossNums.GetEnumerator()
        let downEnum = downNums.GetEnumerator()
        for r in 0..9 do
            for c in 0..9 do
                match readBoard r c with
                | Some '_' -> yield (r, c)
                | _ -> ()
    ]

    let rcToVar r c = vars |> Seq.findIndex(fun (_r, _c) -> r = _r && c = _c)

    let eqns: Equation list = [
        let acrossEnum = acrossNums.GetEnumerator()
        let downEnum = downNums.GetEnumerator()
        for r in 0..9 do
            for c in 0..9 do
                let current = readBoard r c
                let right = readBoard r (c+1)
                let down = readBoard (r+1) c
                if current = Some '.' && right = Some '_' then
                    acrossEnum.MoveNext() |> ignore
                    let cellSeq = [c+1..9] |> List.takeWhile(fun i -> readBoard r i = Some '_') |> List.map(fun i -> rcToVar r i)
                    yield (acrossEnum.Current, cellSeq)
                if current = Some '.' && down = Some '_' then
                    downEnum.MoveNext() |> ignore
                    let cellSeq = [r+1..9] |> List.takeWhile(fun i -> readBoard i c = Some '_') |> List.map(fun i -> rcToVar i c)
                    yield (downEnum.Current, cellSeq)
    ]

    let nextPotentialNumbers (soln: int option list) =
        [1..9] |> List.where(fun i ->
            let possibleSoln = [
                yield! Seq.replicate (Seq.length vars - Seq.length soln - 1) None
                yield Some i
                yield! soln
            ]
            isValidSolutionSoFar eqns  possibleSoln
        )

    let rec getSolutions partialSoln =
        if Seq.length partialSoln = Seq.length vars
        then [partialSoln]
        else 
            nextPotentialNumbers partialSoln
            |> List.map(fun x -> (Some x::partialSoln))
            |> List.collect(getSolutions)

    let soln = Seq.exactlyOne (getSolutions [])
    printfn "%A" soln
    
    let outputFile = Path.Combine(Directory.GetCurrentDirectory(), "output.html")
    let output = DrawBoard.DrawBoard acrossNums downNums (soln |> Seq.map(Option.get >> sprintf "%i")) readBoard
    File.WriteAllLines(outputFile, output)
    ignore <| System.Diagnostics.Process.Start(@"cmd.exe ", @"/c " + outputFile)
    0 // return an integer exit code
