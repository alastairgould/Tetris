module Tetris.Tests.TetrominoTests

open Expecto
open Tetris.Tests.TetrominoTestHelpers
open Tetris.GameModel.Tetromino

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

[<Tests>] 
let oTetrominoTests =
    testList "O tetromino tests" [
        test "Given an empty grid, when a O tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 2 by 2 test grid``
            let tetromino = createOTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1];
                                                       [1; 1]]
        }
    
        test "Given an empty grid, when a O tetromino is placed on the grid and rotated right, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 2 by 2 test grid``
            let tetromino = createOTetromino
                            |> rotateTetrominoRight
                            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given an empty grid, when a O tetromino is placed on the grid and rotated right twice, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 2 by 2 test grid``
            let tetromino = createOTetromino
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given an empty grid, when a O tetromino is placed and rotated right three times, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 2 by 2 test grid``
            let tetromino = createOTetromino
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given an empty grid, when a O tetromino is placed and rotated right four times, then the tetromino should be in the original non rotated position"  {
            let testGrid = ``empty 2 by 2 test grid``
            let tetromino = createOTetromino
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
        
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1];
                                                       [1; 1]]
        }
    ]

[<Tests>] 
let jTetrominoTests =
    testList "J tetromino tests" [
        test "Given an empty grid, when a J tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 3 by 3 test grid``
            let tetromino = createJTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
    
        test "Given an empty grid, when a J tetromino is placed on the grid and rotated right, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 3 by 3 test grid``
            let tetromino = createJTetromino
                            |> rotateTetrominoRight
                            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 1];
                                                       [0; 1; 0];
                                                       [0; 1; 0]]
        }
        
        test "Given an empty grid, when a J tetromino is placed on the grid and rotated right twice, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 3 by 3 test grid``
            let tetromino = createJTetromino
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 1]]
        }
        
        test "Given an empty grid, when a J tetromino is placed and rotated right three times, then the following cells in the grid should have blocks"  {
            let testGrid = ``empty 3 by 3 test grid``
            let tetromino = createJTetromino
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0];
                                                       [0; 1; 0];
                                                       [1; 1; 0]]
        }
        
        test "Given an empty grid, when a J tetromino is placed and rotated right four times, then the tetromino should be in the original non rotated position"  {
            let testGrid = ``empty 3 by 3 test grid``
            let tetromino = createJTetromino
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
                            |> rotateTetrominoRight
            
            let resultingGrid = tetromino |> placeTetrominoInTheDefaultPositionOn testGrid
        
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
    ]
