module Tetris.GameModel.Grid

open Tetris.GameModel.BlockColor

type TetrisGridCell =
    | CellWithBlock of BlockColor
    | CellWithoutBlock
    
type TetrisGridRow = TetrisGridRow of TetrisGridCell list

type TetrisGrid = TetrisGrid of TetrisGridRow list 

let getGridArray (TetrisGrid grid) = grid

let getRowList (TetrisGridRow grid) = grid

let createRow =
    TetrisGridRow [for x in 0 .. 9 -> CellWithoutBlock ]

let removeFilledRows tetrisGrid =
    
    let isRowEmpty (row: TetrisGridRow) =
        let emptyCells = row |> getRowList |> List.filter(fun cell -> cell = CellWithoutBlock)
        emptyCells.Length <> 0
    
    let tetrisGridList = tetrisGrid |> getGridArray
    
    let tetrisGridWithNoFullRows = tetrisGridList |> List.filter(isRowEmpty)
    let amountOfNewRows = 19 - tetrisGridWithNoFullRows.Length
    
    let newRows = [for y in 0 .. amountOfNewRows -> createRow]
    
    tetrisGridWithNoFullRows |> List.rev |> List.append newRows |> List.rev |> TetrisGrid
