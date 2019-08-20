module Tetris.Tests.TetrominoTests

open Expecto
open Tetris.Tests.TetrominoTestHelpers
open Tetris.GameModel.Tetromino
open Tetris.GameModel.BlockColor

[<Tests>] 
let tests =
    testList "I tetromino tests" [
        test "Given a I Tetromino, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createITetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``

            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0; 0];
                                                       [1; 1; 1; 1];
                                                       [0; 0; 0; 0];
                                                       [0; 0; 0; 0]]
        }
        
        test "Given a I Tetromino, when the tetromino is placed on the grid, then all cells with blocks should be light blue"  {
            let tetromino = createITetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``

            resultingGrid |> TetrisExpect.blocksShouldAllHaveColor LightBlue
        }
    
        test "Given I tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createITetromino
                            |> rotateClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 1; 0];
                                                       [0; 0; 1; 0];
                                                       [0; 0; 1; 0];
                                                       [0; 0; 1; 0]]
        }
        
        test "Given I tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createITetromino
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0; 0];
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
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0; 0];
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
        
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0; 0];
                                                       [1; 1; 1; 1];
                                                       [0; 0; 0; 0];
                                                       [0; 0; 0; 0]]
        }
        
        test "Given I tetromino rotated anti clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createITetromino
                            |> rotateAntiClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0; 0];
                                                       [0; 1; 0; 0];
                                                       [0; 1; 0; 0];
                                                       [0; 1; 0; 0]]
        }
        
        test "Given I tetromino rotated anti clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createITetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0; 0];
                                                       [0; 0; 0; 0];
                                                       [1; 1; 1; 1];
                                                       [0; 0; 0; 0]]
        }
        
        test "Given I tetromino rotated anti clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createITetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 1; 0];
                                                       [0; 0; 1; 0];
                                                       [0; 0; 1; 0];
                                                       [0; 0; 1; 0]]
        }
        
        test "Given I tetromino rotated anti clockwise four times, when the tetromino is placed on the grid, then tetromino should be in the original position"  {
            let tetromino = createITetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 4 by 4 test grid``
        
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0; 0];
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
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O Tetromino, when the tetromino is placed on the grid, then all cells with blocks should be yellow"  {
            let tetromino = createOTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``

            resultingGrid |> TetrisExpect.blocksShouldAllHaveColor Yellow
        }
    
        test "Given a O tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createOTetromino
                            |> rotateClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createOTetromino
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createOTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O tetromino rotated clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createOTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
        
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O tetromino rotated anti clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createOTetromino
                            |> rotateAntiClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O tetromino rotated anti clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createOTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O tetromino rotated anti clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createOTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1];
                                                       [1; 1]]
        }
        
        test "Given a O tetromino rotated anti clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createOTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 2 by 2 test grid``
        
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1];
                                                       [1; 1]]
        }
    ]

[<Tests>] 
let jTetrominoTests =
    testList "J tetromino tests" [
        test "Given a J tetromino, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
        
        test "Given a J Tetromino, when the tetromino is placed on the grid, then all cells with blocks should be dark blue"  {
            let tetromino = createJTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``

            resultingGrid |> TetrisExpect.blocksShouldAllHaveColor DarkBlue
        }
    
        test "Given a J tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino
                            |> rotateClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 1];
                                                       [0; 1; 0];
                                                       [0; 1; 0]]
        }
        
        test "Given a J tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 1]]
        }
        
        test "Given a J tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
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
        
            resultingGrid |> TetrisExpect.shouldEqual [[1; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }

        test "Given a J tetromino rotated anti clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino
                            |> rotateAntiClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [0; 1; 0];
                                                       [1; 1; 0]]
        }
        
        test "Given a J tetromino rotated anti clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 1]]
        }
        
        test "Given a J tetromino rotated anti clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createJTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 1];
                                                       [0; 1; 0];
                                                       [0; 1; 0]]
        }
        
        test "Given a J tetromino rotated anti clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createJTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
        
            resultingGrid |> TetrisExpect.shouldEqual [[1; 0; 0];
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
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 1];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
        
        test "Given a L Tetromino, when the tetromino is placed on the grid, then all cells with blocks should be orange"  {
            let tetromino = createLTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``

            resultingGrid |> TetrisExpect.blocksShouldAllHaveColor Orange
        }
    
        test "Given a L tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createLTetromino
                            |> rotateClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [0; 1; 0];
                                                       [0; 1; 1]]
        }
        
        test "Given a L tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createLTetromino
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0];
                                                       [1; 1; 1];
                                                       [1; 0; 0]]
        }
        
        test "Given a L tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createLTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1; 0];
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
        
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 1];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
        
        test "Given a L tetromino rotated anti clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createLTetromino
                            |> rotateAntiClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1; 0];
                                                       [0; 1; 0];
                                                       [0; 1; 0]]
        }
        
        test "Given a L tetromino rotated anti clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createLTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0];
                                                       [1; 1; 1];
                                                       [1; 0; 0]]
        }
        
        test "Given a L tetromino rotated anti clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createLTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [0; 1; 0];
                                                       [0; 1; 1]]
        }
        
        test "Given a L tetromino rotated anti clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createLTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
        
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 1];
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
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 1];
                                                       [1; 1; 0];
                                                       [0; 0; 0]]
        }
        
        test "Given a S Tetromino, when the tetromino is placed on the grid, then all cells with blocks should be green"  {
            let tetromino = createSTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``

            resultingGrid |> TetrisExpect.blocksShouldAllHaveColor Green
        }
    
        test "Given a S tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createSTetromino
                            |> rotateClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 0; 1]]
        }
        
        test "Given a S tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createSTetromino
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0];
                                                       [0; 1; 1];
                                                       [1; 1; 0]]
        }
        
        test "Given a S tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createSTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 0; 0];
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
        
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 1];
                                                       [1; 1; 0];
                                                       [0; 0; 0]]
        }
        
        test "Given a S tetromino rotated anti clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createSTetromino
                            |> rotateAntiClockwise
                            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
            
            resultingGrid |> TetrisExpect.shouldEqual [[1; 0; 0];
                                                       [1; 1; 0];
                                                       [0; 1; 0]]
        }
        
        test "Given a S tetromino rotated anti clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createSTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0];
                                                       [0; 1; 1];
                                                       [1; 1; 0]]
        }
        
        test "Given a S tetromino rotated anti clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createSTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 0; 1]]
        }
        
        test "Given a S tetromino rotated anti clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createSTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
        
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 1];
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
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
        
        test "Given a T Tetromino, when the tetromino is placed on the grid, then all cells with blocks should be purple"  {
            let tetromino = createTTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``

            resultingGrid |> TetrisExpect.blocksShouldAllHaveColor Purple
        }
    
        test "Given a T tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createTTetromino
                            |> rotateClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 1; 0]]
        }
        
        test "Given a T tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createTTetromino
                            |> rotateClockwise
                            |> rotateClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 1; 0]]
        }
        
        test "Given a T tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createTTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
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
        
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [1; 1; 1];
                                                       [0; 0; 0]]
        }
        
        
        test "Given a T tetromino rotated anti clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createTTetromino
                            |> rotateAntiClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [1; 1; 0];
                                                       [0; 1; 0]]
        }
        
        test "Given a T tetromino rotated anti clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createTTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0];
                                                       [1; 1; 1];
                                                       [0; 1; 0]]
        }
        
        test "Given a T tetromino rotated anti clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createTTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 1; 0]]
        }
        
        test "Given a T tetromino rotated anti clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createTTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
        
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
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
                         
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 0; 0]]
        }
        
        test "Given a Z Tetromino, when the tetromino is placed on the grid, then all cells with blocks should be red"  {
            let tetromino = createZTetromino 
        
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``

            resultingGrid |> TetrisExpect.blocksShouldAllHaveColor Red
        }
    
        test "Given a Z tetromino rotated clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createZTetromino
                            |> rotateClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 1];
                                                       [0; 1; 1];
                                                       [0; 1; 0]]
        }
        
        test "Given a Z tetromino rotated clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createZTetromino
                            |> rotateClockwise
                            |> rotateClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0];
                                                       [1; 1; 0];
                                                       [0; 1; 1]]
        }
        
        test "Given a Z tetromino rotated clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createZTetromino
                            |> rotateClockwise
                            |> rotateClockwise
                            |> rotateClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
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
        
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 0; 0]]
        }
        
        test "Given a Z tetromino rotated anti clockwise, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createZTetromino
                            |> rotateAntiClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 1; 0];
                                                       [1; 1; 0];
                                                       [1; 0; 0]]
        }
        
        test "Given a Z tetromino rotated anti clockwise two times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createZTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise

            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 0];
                                                       [1; 1; 0];
                                                       [0; 1; 1]]
        }
        
        test "Given a Z tetromino rotated anti clockwise three times, when the tetromino is placed on the grid, then the following cells in the grid should have blocks"  {
            let tetromino = createZTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
                         
            resultingGrid |> TetrisExpect.shouldEqual [[0; 0; 1];
                                                       [0; 1; 1];
                                                       [0; 1; 0]]
        }
        
        test "Given a Z tetromino rotated anti clockwise four times, when the tetromino is placed on the grid, then the tetromino should be in the original position"  {
            let tetromino = createZTetromino
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
                            |> rotateAntiClockwise
            
            let resultingGrid = tetromino |> placeTetrominoOn ``empty 3 by 3 test grid``
        
            resultingGrid |> TetrisExpect.shouldEqual [[1; 1; 0];
                                                       [0; 1; 1];
                                                       [0; 0; 0]]
        }
    ]
