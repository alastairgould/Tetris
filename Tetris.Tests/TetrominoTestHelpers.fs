module Tetris.Tests.TetrominoTestHelpers

open Tetris.GameModel.Tetromino
open Tetris.GameModel.Grid

let placeTetrominoInTheDefaultPositionOn grid tetromino =
      let tetrominoPosition = { Tetromino = tetromino;
                                Position = createTetrominoPositionCoordinates 2y 2y }
      tetrominoPosition |> addTetrominoToGrid grid

let  ``empty 4 by 4 test grid`` = TetrisGrid ([for y in 0 .. 3 -> [for x in 0 .. 3-> CellWithoutBlock]])
