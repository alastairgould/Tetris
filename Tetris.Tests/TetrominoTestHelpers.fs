module Tetris.Tests.TetrominoTestHelpers

open Tetris.GameModel.Tetromino
open Tetris.GameModel.Grid

let placeTetrominoInTheDefaultPositionOn grid tetromino =
      let tetrominoPosition = { Tetromino = tetromino;
                                Position = createTetrominoPositionCoordinates 0y 0y }
      tetrominoPosition |> addTetrominoToGrid grid

let  ``empty 4 by 4 test grid`` = TetrisGrid ([for y in 0 .. 3 -> [for x in 0 .. 3-> CellWithoutBlock]])

let  ``empty 3 by 3 test grid`` = TetrisGrid ([for y in 0 .. 2 -> [for x in 0 .. 2-> CellWithoutBlock]])

let  ``empty 2 by 2 test grid`` = TetrisGrid ([for y in 0 .. 1 -> [for x in 0 .. 1-> CellWithoutBlock]])
