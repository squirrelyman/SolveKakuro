// Learn more about F# at http://fsharp.org

open System
open System.IO
open ReadBoard

type Equation = int * int list

let minTotalPossible n = n * (n+1) / 2
let maxTotalPossible n = n * (19 - n) / 2

let rec isValidSolutionSoFar (eqns: Equation list) (solution: int list) : bool =
    match eqns with
    | [] -> true
    | (targetSum, vars)::tail ->
        let vals = vars |> List.choose(fun i -> List.tryItem(i) solution)
        let actualSum = vals |> List.sum
        
        let noRepeats = (vals |> List.distinct |> List.length) = vals.Length
        let numEmpty = vars.Length - vals.Length
    
        noRepeats
        && actualSum <= targetSum - (minTotalPossible numEmpty)
        && actualSum >= targetSum - (maxTotalPossible numEmpty)
        && isValidSolutionSoFar tail solution

[<EntryPoint>]
let main argv =
    let board = ReadBoard("../../../Board1.txt")

    let vars: (int*int) list = [
        let acrossEnum = board.acrossSums.GetEnumerator()
        let downEnum = board.downSums.GetEnumerator()
        for r in 0..9 do
            for c in 0..9 do
                match board.cells.[r].[c] with
                | White -> yield (r, c)
                | Black -> ()
    ]

    let rcToVar r c = vars |> Seq.findIndex(fun (_r, _c) -> r = _r && c = _c)
    let tryCell r c = board.cells |> List.tryItem(r) |> Option.map(List.tryItem(c)) |> Option.flatten

    let eqns: Equation list = [
        let acrossEnum = board.acrossSums.GetEnumerator()
        let downEnum = board.downSums.GetEnumerator()
        for r in 0..9 do
            for c in 0..9 do
                let current = tryCell r c
                let right = tryCell r (c+1)
                let down = tryCell (r+1) c
                if current = Some Black && right = Some White then
                    acrossEnum.MoveNext() |> ignore
                    let cellSeq = [c+1..9] |> List.takeWhile(fun i -> tryCell r i = Some White) |> List.map(fun i -> rcToVar r i)
                    yield (acrossEnum.Current, cellSeq)
                if current = Some Black && down = Some White then
                    downEnum.MoveNext() |> ignore
                    let cellSeq = [r+1..9] |> List.takeWhile(fun i -> tryCell i c = Some White) |> List.map(fun i -> rcToVar i c)
                    yield (downEnum.Current, cellSeq)
    ]

    //runs faster with equations reversed
    let eqns = List.rev eqns

    let rec getSolutions partialSoln =
        if Seq.length partialSoln = Seq.length vars
        then [partialSoln]
        else 
            [1..9]
            |> List.map(fun i -> (i::partialSoln))
            |> List.where(List.rev >> isValidSolutionSoFar eqns)
            |> List.collect(getSolutions)

    let soln = getSolutions [] |> Seq.exactlyOne |> List.rev
    printfn "%A" soln
    
    let outputFile = Path.Combine(Directory.GetCurrentDirectory(), "output.html")
    let output = DrawBoard.DrawBoard board (soln |> Seq.map(sprintf "%i"))
    File.WriteAllLines(outputFile, output)
    ignore <| System.Diagnostics.Process.Start(@"cmd.exe ", @"/c " + outputFile)
    0 // return an integer exit code
