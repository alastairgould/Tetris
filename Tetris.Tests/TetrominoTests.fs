module Tests

open Expecto
open Expecto.Flip
open Tetris.GameModel.Tetromino
open Tetris.GameModel.Grid

let private convertToSimpleGrid grid =
    let getGrid(TetrisGrid grid) = grid
    let grid = getGrid grid 
    let returnBoolRow (row: TetrisGridCell list) = row |> List.map(fun cell -> match cell with
                                                                                | CellWithBlock _ -> 1
                                                                                | _ -> 0)
    grid |> List.map(returnBoolRow) 

let private ``empty 4 by 4 test grid`` = TetrisGrid ([for y in 0 .. 3 -> [for x in 0 .. 3-> CellWithoutBlock]])

let private placeTetrominoInTheDefaultPositionOn grid tetromino =
      let tetrominoPosition = { Tetromino = tetromino;
                                Position = createTetrominoPositionCoordinates 2y 2y }
      tetrominoPosition |> addTetrominoToGrid grid
      
module TetrisExpect = 
    let ShouldEqual expected actual =
        let actualReversed = actual |> convertToSimpleGrid
        let expectedReversed = expected |> List.rev
        actualReversed |> Expect.equal "The resultant grid should be expected to equal " expectedReversed

[<Tests>]
let tests =
    testList "I tetromino tests" [
        test "Given an empty grid, when a I tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 4 by 4 test grid``
            let tetromino = createITetromino 
        
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0; 0];
                                                       [1; 1; 1; 1];
                                                       [0; 0; 0; 0];
                                                       [0; 0; 0; 0]]
        }
    
        test "Given an empty grid, when a I tetromino is placed on the grid and rotated right, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 4 by 4 test grid``
            let tetromino = createITetromino
                            |> rotateTetrominoRight
                            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 1; 0];
                                                       [0; 0; 1; 0];
                                                       [0; 0; 1; 0];
                                                       [0; 0; 1; 0]]
        }
        
        test "Given an empty grid, when a I tetromino is placed on the grid and rotated right twice, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 4 by 4 test grid``
            let tetromino = createITetromino
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0; 0];
                                                       [0; 0; 0; 0];
                                                       [1; 1; 1; 1];
                                                       [0; 0; 0; 0]]
        }
        
        test "Given an empty grid, when a I tetromino is placed and rotated right three times, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 4 by 4 test grid``
            let tetromino = createITetromino
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0; 0];
                                                       [0; 1; 0; 0];
                                                       [0; 1; 0; 0];
                                                       [0; 1; 0; 0]]
        }
        
        test "Given an empty grid, when a I tetromino is placed and rotated right four times, then the tetromino should be in the original non rotated position"  {
            let testGrid = ``empty 4 by 4 test grid``
            let tetromino = createITetromino
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
        
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0; 0];
                                                       [1; 1; 1; 1];
                                                       [0; 0; 0; 0];
                                                       [0; 0; 0; 0]]
        }
    ]
