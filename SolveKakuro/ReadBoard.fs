module ReadBoard
open System.IO
open System

type CellType =
    | White
    | Black

type Board =
    {
        cells: CellType list list
        acrossSums: int seq
        downSums: int seq
    }

let ReadBoard fileName =
    let lines = File.ReadLines(fileName)
    let cells =
        [0..9] |> List.map(fun r ->
            [0..9] |> List.map(fun c ->
                match lines |> Seq.item(r) |> Seq.item(c) with
                | '_' -> White
                | '.' -> Black
                | _ -> failwith "Unexpected char in board"
    ))

    {
        cells = cells
        acrossSums = (lines |> Seq.item(10)).Split(' ') |> Seq.map(Int32.Parse)
        downSums = (lines |> Seq.item(11)).Split(' ') |> Seq.map(Int32.Parse)
    }
