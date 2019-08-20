module Tetris.GameModel.GameModel

open Tetris.GameModel.Collision
open Tetris.GameModel.Grid
open Tetris.GameModel.Tetromino

type GameModel = private {
    Tetromino: TetrominoWithPosition
    Grid: TetrisGrid
}

type CurrentStepState =
    private
        | ReactedToInput of GameModel
        | NotReactedToInput of GameModel

type InputToGameModel =
    | MoveLeft
    | MoveRight
    | RotateClockwise

let private  getGameModelFromStepState stepState =
    match stepState with
        | ReactedToInput state -> state
        | NotReactedToInput state -> state

let renderableGrid stepState =
    let gameModel = stepState |> getGameModelFromStepState 
    addTetrominoToGrid gameModel.Grid gameModel.Tetromino

let createInitialGameModel = 
    let tetrominoWithPosition = { Position = createTetrominoPositionCoordinates 3y 17y; Tetromino = createTetromino() }
    let grid = createEmptyGrid
    NotReactedToInput { Tetromino = tetrominoWithPosition; Grid = grid }
    
let bind f stepState =
    match stepState with
        | ReactedToInput _ -> stepState
        | NotReactedToInput gameModel -> f gameModel

let handleInputRotateClockwise gameModel =
    let tetrominoWithPosition = gameModel.Tetromino
    let rotatedTetromino = rotateClockwise tetrominoWithPosition.Tetromino
    let newGameModel = { gameModel with Tetromino = { tetrominoWithPosition with Tetromino = rotatedTetromino} }
    
    let outcome = checkMovement newGameModel.Grid newGameModel.Tetromino Left
    
    match outcome with
        | TakeAction -> ReactedToInput newGameModel
        | CantTakeAction -> NotReactedToInput gameModel
        | TetrominoPlaced -> NotReactedToInput gameModel

let handleInputMoveLeft (gameModel: GameModel) =
    let newGameModel = { gameModel with Tetromino = gameModel.Tetromino + (createTetrominoVelocity -1y 0y) }
    let outcome = checkMovement newGameModel.Grid newGameModel.Tetromino Left
    
    match outcome with
        | TakeAction -> ReactedToInput newGameModel
        | CantTakeAction -> NotReactedToInput gameModel
        | TetrominoPlaced -> NotReactedToInput gameModel

let handleInputMoveRight (gameModel: GameModel) =
    let newGameModel = { gameModel with Tetromino = gameModel.Tetromino + (createTetrominoVelocity 1y 0y) }
    let outcome = checkMovement newGameModel.Grid newGameModel.Tetromino Right
    
    match outcome with
        | TakeAction -> ReactedToInput newGameModel
        | CantTakeAction -> NotReactedToInput gameModel
        | TetrominoPlaced -> NotReactedToInput gameModel

let reactToInput (stepState: CurrentStepState) (input: InputToGameModel) =
    let handleInput gameModel = match input with 
                                    | MoveLeft -> handleInputMoveLeft gameModel
                                    | MoveRight -> handleInputMoveRight gameModel
                                    | RotateClockwise -> handleInputRotateClockwise gameModel
    
    bind handleInput stepState

let newTetromino() = 
    { Position = createTetrominoPositionCoordinates 3y 17y; Tetromino = createTetromino() }

let stepWorld (stepState: CurrentStepState) =
    let gameModel = stepState |> getGameModelFromStepState
    let newGameModel =  { gameModel with Tetromino = gameModel.Tetromino + (createTetrominoVelocity 0y -1y) }
    let outcome = checkMovement newGameModel.Grid newGameModel.Tetromino Down
    
    let placeTetromino =
        let placedGrid = { Grid = (addTetrominoToGrid gameModel.Grid gameModel.Tetromino); Tetromino = newTetromino()}
        { placedGrid with Grid = removeFilledRows placedGrid.Grid } 
        
    match outcome with
        | TetrominoPlaced -> NotReactedToInput placeTetromino
        | CantTakeAction -> NotReactedToInput placeTetromino
        | TakeAction -> NotReactedToInput newGameModel 
