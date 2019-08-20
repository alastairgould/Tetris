module Tetris.GameModel.Grid

open Tetris.GameModel.BlockColor

let private height = 20
let private width = 10

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

let private createEmptyRow =
    TetrisGridRow [for x in 1 .. width -> CellWithoutBlock]
    
let private isRowFull row =
    let emptyCells = row |> getRowList |> List.filter(fun cell -> cell = CellWithoutBlock)
    emptyCells.Length <> 0

let createEmptyGrid =
    TetrisGrid [for x in 1 .. height -> createEmptyRow]

let removeFilledRows tetrisGrid =
    let tetrisGridList = tetrisGrid |> getGridArray
    let fullRows = tetrisGridList |> List.filter(fun row -> not (row |> isRowFull))
    let tetrisGridWithNoFullRows = tetrisGridList |> List.filter(isRowFull)
    let newEmptyRows = fullRows |> List.map(fun _ -> createEmptyRow)
    tetrisGridWithNoFullRows |> List.rev |> List.append newEmptyRows |> List.rev |> TetrisGrid
