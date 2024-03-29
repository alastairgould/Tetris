module Tetris.GameModel.Grid

open Tetris.GameModel.BlockColor

let height, width = 20y, 10y

type TetrisGridCell =
    | CellWithBlock of BlockColor
    | CellWithoutBlock
    
type TetrisGridRow = TetrisGridRow of TetrisGridCell list

type TetrisGrid = TetrisGrid of TetrisGridRow list 

let getGridArray (TetrisGrid grid) = grid

let getRowList (TetrisGridRow grid) = grid

let getCellColor cell =
    match cell with
        | CellWithBlock color -> Some color
        | CellWithoutBlock -> None 

let private isRowFull row =
    let emptyCells = row |> getRowList |> List.filter(fun cell -> cell = CellWithoutBlock)
    emptyCells.Length <> 0

let private createEmptyRow =
    TetrisGridRow [for x in 1y .. width -> CellWithoutBlock]
    
let createEmptyGrid =
    TetrisGrid [for y in 1y .. height -> createEmptyRow]

let removeFilledRows tetrisGrid =
    let tetrisGridList = tetrisGrid |> getGridArray
    let rowsWhichAreFull = tetrisGridList |> List.filter(fun row -> not (row |> isRowFull))
    let tetrisRowsWithFullRowsRemoved = tetrisGridList |> List.filter(isRowFull)
    let newEmptyRowsToReplaceRemovedRows = rowsWhichAreFull |> List.map(fun _ -> createEmptyRow)
    tetrisRowsWithFullRowsRemoved |> List.rev |> List.append newEmptyRowsToReplaceRemovedRows |> List.rev |> TetrisGrid
