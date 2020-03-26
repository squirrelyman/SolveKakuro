// Learn more about F# at http://fsharp.org

open System
open System.IO
open ReadBoard

//Equation(sum, variable indices)
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

    let vars =
        Seq.allPairs [0..9] [0..9]
        |> Seq.filter(fun (r, c) -> board.cells.[r].[c] = White)

    let rcToVar r c = vars |> Seq.findIndex(fun (_r, _c) -> r = _r && c = _c)
    let tryCell r c = board.cells |> List.tryItem(r) |> Option.map(List.tryItem(c)) |> Option.flatten

    let acrossRuns =
        vars
        |> Seq.filter(fun(r, c) -> tryCell r (c-1) = Some Black)
        |> Seq.map(fun (r, c) ->
            [c..9]
            |> List.takeWhile(fun i -> tryCell r i = Some White)
            |> List.map(fun i -> rcToVar r i))

    let downRuns =
        vars
        |> Seq.filter(fun(r, c) -> tryCell (r-1) c = Some Black)
        |> Seq.map(fun (r, c) ->
            [r..9]
            |> List.takeWhile(fun i -> tryCell i c = Some White)
            |> List.map(fun i -> rcToVar i c))

    let eqns = [
        yield! Seq.zip board.acrossSums acrossRuns
        yield! Seq.zip board.downSums downRuns
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
