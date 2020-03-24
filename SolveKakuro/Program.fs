// Learn more about F# at http://fsharp.org

open System
open System.IO

type Equation = int * int list

let rec permutations (myLists: int list list): int list list =
    match myLists with
    | [] -> failwith ""
    | [x] -> x |> List.map(fun y -> [y])
    | h::t -> List.allPairs h (permutations t) |> List.map(fun (x, y) -> x::y)

let isSolution (eqn: Equation) (vals: int list) =
    let sum, vars = eqn
    (vals |> Seq.length) = (vals |> Seq.distinct |> Seq.length)
    && (Seq.length vars) = (vals |> Seq.length)
    && (Seq.sum vals) = sum

let isFullSolution (eqns: Equation list) (solution: int list) =
    eqns
    |> Seq.map(fun eqn ->
        let _, vars = eqn
        let vals = vars |> List.map(fun i -> solution.[i])
        isSolution eqn vals
    ) |> Seq.reduce(fun x y -> x && y)

let isSolutionMultiple (eqns: Equation list) (vals: int list list) =
    Seq.zip eqns vals |> Seq.map(fun(eqn, vals) -> isSolution eqn vals) |> Seq.reduce(fun x y -> x && y)

let pruneSolutions (eqns: Equation list) (solutions: int list list) =
    [0..Seq.length solutions-1]
    |> List.map(fun var ->
        let relevantEqns = eqns |> List.filter(fun (_, vars) -> List.contains(var) vars)
        let relevantVars = relevantEqns |> List.collect(fun (_, vars) -> vars) |> List.distinct 
        let possibleValues = relevantVars |> List.map(fun i -> solutions.[i]) |> permutations
        let possibleSolutions = possibleValues |> List.filter(fun possiblePermutation ->
            let mockFullSolution =
                [0..Seq.length solutions - 1] |> List.map(fun i ->
                    match List.tryFindIndex(fun x -> i = x) relevantVars with
                    | Some x -> possiblePermutation.[x]
                    | _ -> 0
            )
            isFullSolution relevantEqns mockFullSolution
        )
        (*let sum, vars = relevantEqn.[0]
        let possibleSolutions = vars |> List.map(fun i -> solutions.[i]) |> permutations
        
        let possibleSolutions = possibleSolutions |> List.filter(isSolution relevantEqn)*)
        let varIndex = List.findIndex(fun x -> x = var) relevantVars
        possibleSolutions |> List.map(fun l -> l.[varIndex]) |> List.distinct
    )

let pruneSolutions1 (eqns: Equation list) (solutions: int list list) =
    [0..Seq.length solutions-1]
    |> List.map(fun var ->
        let relevantEqns = eqns |> List.filter(fun (_, vars) -> List.contains(var) vars)
        let relevantEqns = relevantEqns |> List.take(1)
        let relevantVars = relevantEqns |> List.collect(fun (_, vars) -> vars) |> List.distinct 
        let possibleValues = relevantVars |> List.map(fun i -> solutions.[i]) |> permutations
        let possibleSolutions = possibleValues |> List.filter(fun possiblePermutation ->
            let mockFullSolution =
                [0..Seq.length solutions - 1] |> List.map(fun i ->
                    match List.tryFindIndex(fun x -> i = x) relevantVars with
                    | Some x -> possiblePermutation.[x]
                    | _ -> 0
            )
            isFullSolution relevantEqns mockFullSolution
        )
        (*let sum, vars = relevantEqn.[0]
        let possibleSolutions = vars |> List.map(fun i -> solutions.[i]) |> permutations
        
        let possibleSolutions = possibleSolutions |> List.filter(isSolution relevantEqn)*)
        let varIndex = List.findIndex(fun x -> x = var) relevantVars
        possibleSolutions |> List.map(fun l -> l.[varIndex]) |> List.distinct
    )


let potentialSolutions (eqn: Equation) =
    let sum, vars = eqn
    let nVars = Seq.length vars

    let rec combinations firstVal numsNeeded : int list seq =
        match numsNeeded with
        | 1 -> [firstVal..9] |> Seq.map(fun x -> [x])
        | n -> 
            [firstVal..9]
            |> Seq.collect(fun (x: int) ->
                combinations (x+1) (n-1)
                |> Seq.map(fun y -> x::y)
            )
        
    combinations 1 nVars |> Seq.filter(fun combo -> Seq.sum combo = sum)

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

    let solutions = vars |> List.map(fun _ -> [1..9]) |> pruneSolutions1 eqns
    let solutions = solutions |> pruneSolutions eqns

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

        for eqn in eqns do
            let (total, vars) = eqn
            let str = vars |> Seq.map(sprintf "a%i") |> Seq.reduce (fun x y -> x + " + " + y)
            yield sprintf "<p>%i = %s Possibilities %A</p>" total str (potentialSolutions eqn)

        yield "<p>Solutions</p>"
        yield! solutions |> Seq.mapi(fun i varVals ->
            let valsStr = varVals |> Seq.map(sprintf "%i") |> Seq.reduce (fun x y -> x + " " + y)
            sprintf "<p>a%i: %s</p>" i valsStr
        )
    ]
    
    let outputFile = Path.Combine(Directory.GetCurrentDirectory(), "output.html")
    File.WriteAllLines(outputFile, output)
    ignore <| System.Diagnostics.Process.Start(@"cmd.exe ", @"/c " + outputFile)
    printfn"</table>"
    0 // return an integer exit code
