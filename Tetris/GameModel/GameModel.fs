module Tetris.GameModel.GameModel

open Tetris.GameModel.Grid
open Tetris.GameModel.Tetromino

type GameModel = private {
    Tetromino: TetrominoWithPosition
    Grid: TetrisGrid
}

type CurrentStepState =
    | ReactedToInput of GameModel
    | NotReactedToInput of GameModel

type InputToGameModel =
    | MoveLeft
    | MoveRight

let getGameModelFromStepState stepState =
    match stepState with
        | ReactedToInput state -> state
        | NotReactedToInput state -> state

let renderableGrid stepState =
    let gameModel = stepState |> getGameModelFromStepState 
    addTetrominoToGrid gameModel.Grid gameModel.Tetromino

let createInitialGameModel = 
    let tetrominoWithPosition = { Position = createTetrominoPositionCoordinates 2y 0y; Tetromino = createTetromino; }
    let grid = TetrisGrid ([for y in 0 .. 19 -> [for x in 0 .. 9-> CellWithoutBlock]])
    NotReactedToInput { Tetromino = tetrominoWithPosition; Grid = grid }
    
let stepStateBind f stepState =
    match stepState with
        | ReactedToInput _ -> stepState
        | NotReactedToInput gameModel -> f gameModel
    
let reactToInput (stepState: CurrentStepState) (input: InputToGameModel) =
    let handleInput gameModel = match input with 
                                    | MoveLeft -> ReactedToInput { gameModel with Tetromino = gameModel.Tetromino + (createTetrominoMovement -1y 0y) }
                                    | MoveRight -> ReactedToInput { gameModel with Tetromino = gameModel.Tetromino + (createTetrominoMovement 1y 0y) }
    
    stepStateBind handleInput stepState
    
let stepWorld (stepState: CurrentStepState) =
    let gameModel = stepState |> getGameModelFromStepState
    NotReactedToInput { gameModel with Tetromino = gameModel.Tetromino + (createTetrominoMovement 0y 1y) }
