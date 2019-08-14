module Tetris.Tests.TetrominoTests

open Expecto
open Tetris.Tests.TetrominoTestHelpers
open Tetris.GameModel.Tetromino

[<Tests>] 
let tests =
    testList "I tetromino tests" [
        test "Given a I Tetromino, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createITetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``

            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0; 0];
                                                       [1; 1; 1; 1];
                                                       [0; 0; 0; 0];
                                                       [0; 0; 0; 0]]
        }
    
        test "Given I tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createITetromino
                            |> rotateClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 1; 0];
                                                       [0; 0; 1; 0];
                                                       [0; 0; 1; 0];
                                                       [0; 0; 1; 0]]
        }
        
        test "Given I tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createITetromino
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0; 0];
                                                       [0; 0; 0; 0];
                                                       [1; 1; 1; 1];
                                                       [0; 0; 0; 0]]
        }
        
        test "Given I tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createITetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0; 0];
                                                       [0; 1; 0; 0];
                                                       [0; 1; 0; 0];
                                                       [0; 1; 0; 0]]
        }
        
        test "Given I tetromino rotated clockwise four times, when the tetromino is placed on the grid, then tetromino should be in the original position"  {
            let tetromino = createITetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``
        
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0; 0];
                                                       [1; 1; 1; 1];
                                                       [0; 0; 0; 0];
                                                       [0; 0; 0; 0]]
        }
    ]

[<Tests>] 
let oTetrominoTests =
    testList "O tetromino tests" [
        test "Given a O tetromino, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createOTetromino 
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1];
                                                       [1; 1]]
        }
    
        test "Given a O tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createOTetromino
                            |> rotateClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createOTetromino
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createOTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O tetromino rotated clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the orignal position"  {
            let tetromino = createOTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
        
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1];
                                                       [1; 1]]
        }
    ]

[<Tests>] 
let jTetrominoTests =
    testList "J tetromino tests" [
        test "Given a J tetromino, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
    
        test "Given a J tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino
                            |> rotateClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 1];
                                                       [0; 1; 0];
                                                       [0; 1; 0]]
        }
        
        test "Given a J tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 1]]
        }
        
        test "Given a J tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0];
                                                       [0; 1; 0];
                                                       [1; 1; 0]]
        }
        
        test "Given a J tetromino rotated clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createJTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
        
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
    ]

[<Tests>] 
let lTetrominoTests =
    testList "L tetromino tests" [
        test "Given a L tetromino, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createLTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 1];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
    
        test "Given a L tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createLTetromino
                            |> rotateClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0];
                                                       [0; 1; 0];
                                                       [0; 1; 1]]
        }
        
        test "Given a L tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createLTetromino
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0];
                                                       [1; 1; 1];
                                                       [1; 0; 0]]
        }
        
        test "Given a L tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createLTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1; 0];
                                                       [0; 1; 0];
                                                       [0; 1; 0]]
        }
        
        test "Given a L tetromino rotated clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createLTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
        
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 1];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
    ]
 
[<Tests>] 
let sTetrominoTests =
    testList "S tetromino tests" [
        test "Given a S tetromino, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createSTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 1];
                                                       [1; 1; 0];
                                                       [0; 0; 0]]
        }
    
        test "Given a S tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createSTetromino
                            |> rotateClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 0; 1]]
        }
        
        test "Given a S tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createSTetromino
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0];
                                                       [0; 1; 1];
                                                       [1; 1; 0]]
        }
        
        test "Given a S tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createSTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 0; 0];
                                                       [1; 1; 0];
                                                       [0; 1; 0]]
        }
        
        test "Given a S tetromino rotated clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createSTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
        
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 1];
                                                       [1; 1; 0];
                                                       [0; 0; 0]]
        }
    ]

[<Tests>] 
let tTetrominoTests =
    testList "T tetromino tests" [
        test "Given a T tetromino, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createTTetromino 

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
    
        test "Given a T tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createTTetromino
                            |> rotateClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 1; 0]]
        }
        
        test "Given a T tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createTTetromino
                            |> rotateClockwise
                            |> rotateClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 1; 0]]
        }
        
        test "Given a T tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createTTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0];
                                                       [1; 1; 0];
                                                       [0; 1; 0]]
        }
        
        test "Given a T tetromino rotated clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createTTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
        
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
    ]
    
[<Tests>] 
let zTetrominoTests =
    testList "Z tetromino tests" [
        test "Given a Z tetromino, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createZTetromino 

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 0; 0]]
        }
    
        test "Given a Z tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createZTetromino
                            |> rotateClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 1];
                                                       [0; 1; 1];
                                                       [0; 1; 0]]
        }
        
        test "Given a Z tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createZTetromino
                            |> rotateClockwise
                            |> rotateClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 0; 0];
                                                       [1; 1; 0];
                                                       [0; 1; 1]]
        }
        
        test "Given a Z tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createZTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.ShouldEqual [[0; 1; 0];
                                                       [1; 1; 0];
                                                       [1; 0; 0]]
        }
        
        test "Given a Z tetromino rotated clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createZTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
        
            resultingGrid |> TetrisExpect.ShouldEqual [[1; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 0; 0]]
        }
    ]
