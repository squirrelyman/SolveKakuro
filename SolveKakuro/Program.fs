// Learn more about F# at http://fsharp.org

open System
open System.IO

type Equation = int * int list

let minTotalPossible n = n * (n+1) / 2
let maxTotalPossible n = n * (19 - n) / 2

let isValidSolutionSoFar (eqns: Equation list) (solution: int option list) =
    eqns
    |> Seq.map(fun (targetSum, vars) ->
        let vals = vars |> List.map(fun i -> solution.[i])
        let actualSum = vals |> List.map(Option.defaultValue 0) |> List.sum

        let noRepeats = (vals |> Seq.where(Option.isSome) |> Seq.distinct |> Seq.length) = (vals |> Seq.where(Option.isSome) |> Seq.length)
        let numEmpty = vals |> Seq.where(Option.isNone) |> Seq.length

        noRepeats && actualSum <= targetSum - (minTotalPossible numEmpty) && actualSum >= targetSum - (maxTotalPossible numEmpty)
    ) |> Seq.reduce(fun x y -> x && y)

[<EntryPoint>]
let main argv =
    let board = File.ReadLines("../../../Board1.txt")

    let acrossNums = (board |> Seq.item(10)).Split(' ') |> Seq.map(Int32.Parse)
    let downNums = (board |> Seq.item(11)).Split(' ') |> Seq.map(Int32.Parse)

    assert(Seq.sum acrossNums = Seq.sum downNums)

    let readBoard r c = board |> Seq.tryItem(r) |> Option.map(Seq.tryItem(c)) |> Option.flatten

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

    let output = [
        let acrossEnum = acrossNums.GetEnumerator()
        let downEnum = downNums.GetEnumerator()
        yield
            """
            <style>
            table, th, td {
              border: 1px solid black;
              margin: 0px;
              padding: 0px;
              border-spacing: 0px;
            }
            td {
              width: 30px;
              height: 40px;
            }
            .filled {
              background-color: black;
            }
            .across-total {
              vertical-align: top;
              text-align: right;
              color: white;
            }
            .down-total {
              vertical-align: bottom;
              text-align: left;
              color: white;
            }
            .cell-var {
              color: gray;
            }
            </style>
            """

        yield "<table>"
        for r in 0..9 do
            yield "<tr>"
            for c in 0..9 do
                match readBoard r c with
                | Some '_' ->
                    let cellVar = sprintf "<div class=\"cell-var\">a%i</div>" (rcToVar r c)
                    yield sprintf "<td bgcolor=\"white\" width=\"30\" height=\"40\" border=\"1px solid black\">%s</td>" cellVar
                | Some '.' ->
                    let acrossDiv =
                        match board |> Seq.item(r) |> Seq.tryItem(c+1) with
                        | Some('_') ->
                            acrossEnum.MoveNext() |> ignore
                            let n = acrossEnum.Current
                            sprintf "<div class=\"across-total\">%i</div>" n
                        | _ -> ""
                    let downDiv =
                        match board |> Seq.tryItem(r+1) |> Option.map(Seq.item(c)) with
                        | Some('_') ->
                            downEnum.MoveNext() |> ignore
                            let n = downEnum.Current
                            sprintf "<div class=\"down-total\">%i</div>" n
                        | _ -> ""
                    yield sprintf "<td class=\"filled\">%s%s</td>" acrossDiv downDiv
                | _ -> ()
            yield "</tr>"
        yield "</table>"
        yield sprintf "%i = %i" (Seq.sum acrossNums) (Seq.sum downNums)
    ]

    let nextPotentialNumbers (soln: int option list) =
        [1..9] |> List.where(fun i ->
            let possibleSoln = [
                yield! soln
                yield Some i
                yield! Seq.replicate (Seq.length vars - Seq.length soln - 1) None
            ]
            isValidSolutionSoFar eqns  possibleSoln
        )
        
    let rec solve partialSoln =
        nextPotentialNumbers (partialSoln |> List.rev)
        |> Seq.iter(fun x ->
            solve (Some x::partialSoln)
            printfn "%A" (Some x::partialSoln)
        )

    //solve []

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
            |> List.map(fun x ->
                //printfn "%A" x
                x
            ) |> List.collect(getSolutions)

    printfn "%A" (getSolutions [])
    
    let outputFile = Path.Combine(Directory.GetCurrentDirectory(), "output.html")
    File.WriteAllLines(outputFile, output)
    ignore <| System.Diagnostics.Process.Start(@"cmd.exe ", @"/c " + outputFile)
    printfn"</table>"
    0 // return an integer exit code
