module DrawBoard

let DrawBoard (acrossNums: int seq) (downNums: int seq) (contents: string seq) (readBoard: int -> int -> char option) = 
    [
        let acrossEnum = acrossNums.GetEnumerator()
        let downEnum = downNums.GetEnumerator()
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
                match readBoard r c with
                | Some '_' ->
                    contentsEnum.MoveNext() |> ignore
                    let cellVar = sprintf "<div class=\"cell-var\">%s</div>" contentsEnum.Current
                    yield sprintf "<td bgcolor=\"white\" width=\"30\" height=\"40\" border=\"1px solid black\">%s</td>" cellVar
                | Some '.' ->
                    let acrossDiv =
                        match readBoard r (c+1) with
                        | Some('_') ->
                            acrossEnum.MoveNext() |> ignore
                            let n = acrossEnum.Current
                            sprintf "<div class=\"across-total\">%i</div>" n
                        | _ -> ""
                    let downDiv =
                        match readBoard (r+1) c with
                        | Some('_') ->
                            downEnum.MoveNext() |> ignore
                            let n = downEnum.Current
                            sprintf "<div class=\"down-total\">%i</div>" n
                        | _ -> ""
                    yield sprintf "<td class=\"filled\">%s%s</td>" acrossDiv downDiv
                | _ -> ()
            yield "</tr>"
        yield "</table>"
    ]