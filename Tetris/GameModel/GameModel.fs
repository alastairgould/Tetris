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

let private  getGameModelFromStepState stepState =
    match stepState with
        | ReactedToInput state -> state
        | NotReactedToInput state -> state

let renderableGrid stepState =
    let gameModel = stepState |> getGameModelFromStepState 
    addTetrominoToGrid gameModel.Grid gameModel.Tetromino

let createInitialGameModel = 
    let tetrominoWithPosition = { Position = createTetrominoPositionCoordinates 5y 10y; Tetromino = createTetromino }
    let grid = TetrisGrid ([for y in 0 .. 19 -> TetrisGridRow [for x in 0 .. 9-> CellWithoutBlock]])
    NotReactedToInput { Tetromino = tetrominoWithPosition; Grid = grid }
    
let bind f stepState =
    match stepState with
        | ReactedToInput _ -> stepState
        | NotReactedToInput gameModel -> f gameModel
    
let reactToInput (stepState: CurrentStepState) (input: InputToGameModel) =
    let handleInput gameModel = match input with 
                                    | MoveLeft -> ReactedToInput { gameModel with Tetromino = gameModel.Tetromino + (createTetrominoVelocity -1y 0y) }
                                    | MoveRight -> ReactedToInput { gameModel with Tetromino = gameModel.Tetromino + (createTetrominoVelocity 1y 0y) }
    
    bind handleInput stepState
    
let stepWorld (stepState: CurrentStepState) =
    let gameModel = stepState |> getGameModelFromStepState
    NotReactedToInput { gameModel with Tetromino = gameModel.Tetromino + (createTetrominoVelocity 0y 0y) }
