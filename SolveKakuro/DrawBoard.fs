module DrawBoard
open ReadBoard

let DrawBoard (board: Board) (contents: string seq) = 
    let tryCell r c = board.cells |> List.tryItem(r) |> Option.map(List.tryItem(c)) |> Option.flatten

    [
        let acrossEnum = board.acrossSums.GetEnumerator()
        let downEnum = board.downSums.GetEnumerator()
        let contentsEnum = contents.GetEnumerator()
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
              color: black;
              text-align: center;
            }
            </style>
            """

        yield "<table>"
        for r in 0..9 do
            yield "<tr>"
            for c in 0..9 do
                match board.cells.[r].[c] with
                | White ->
                    contentsEnum.MoveNext() |> ignore
                    let cellVar = sprintf "<div class=\"cell-var\">%s</div>" contentsEnum.Current
                    yield sprintf "<td bgcolor=\"white\" width=\"30\" height=\"40\" border=\"1px solid black\">%s</td>" cellVar
                | Black ->
                    let acrossDiv =
                        match tryCell r (c+1) with
                        | Some White ->
                            acrossEnum.MoveNext() |> ignore
                            let n = acrossEnum.Current
                            sprintf "<div class=\"across-total\">%i</div>" n
                        | _ -> ""
                    let downDiv =
                        match tryCell (r+1) c with
                        | Some White ->
                            downEnum.MoveNext() |> ignore
                            let n = downEnum.Current
                            sprintf "<div class=\"down-total\">%i</div>" n
                        | _ -> ""
                    yield sprintf "<td class=\"filled\">%s%s</td>" acrossDiv downDiv
            yield "</tr>"
        yield "</table>"
    ]