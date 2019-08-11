module Tetris.Grid

open Tetris.BlockColor

type TetrisGridCell =
    | CellWithBlock of BlockColor
    | CellWithoutBlock

type TetrisGrid = TetrisGrid of (TetrisGridCell list) list
