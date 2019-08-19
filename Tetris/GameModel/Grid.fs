module Tetris.GameModel.Grid

open Tetris.GameModel.BlockColor

type TetrisGridCell =
    | CellWithBlock of BlockColor
    | CellWithoutBlock
    
type TetrisGridRow = TetrisGridRow of TetrisGridCell list

type TetrisGrid = TetrisGrid of TetrisGridRow list 

let getGridArray (TetrisGrid grid) = grid

let getRowList (TetrisGridRow grid) = grid
