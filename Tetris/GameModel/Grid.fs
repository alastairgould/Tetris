module Tetris.GameModel.Grid

open Tetris.GameModel.BlockColor

type TetrisGridCell =
    | CellWithBlock of BlockColor
    | CellWithoutBlock

type TetrisGrid = TetrisGrid of (TetrisGridCell list) list
