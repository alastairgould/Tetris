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
    | RotateAntiClockwise

let private  getGameModelFromStepState stepState =
    match stepState with
        | ReactedToInput state -> state
        | NotReactedToInput state -> state

let private newTetromino() = { Position = createTetrominoPositionCoordinates 3y 17y; Tetromino = createTetromino() }

let renderableGrid stepState =
    let gameModel = stepState |> getGameModelFromStepState 
    addTetrominoToGrid gameModel.Grid gameModel.Tetromino

let createInitialGameModel = 
    let tetrominoWithPosition = newTetromino()
    let grid = createEmptyGrid
    NotReactedToInput { Tetromino = tetrominoWithPosition; Grid = grid }
    
let bind f stepState =
    match stepState with
        | ReactedToInput _ -> stepState
        | NotReactedToInput gameModel -> f gameModel

let handleMovementOutcome newGameModel gameModel outcome =
    match outcome with
        | TakeAction -> ReactedToInput newGameModel
        | CantTakeAction -> NotReactedToInput gameModel

let private handleInputRotate rotation gameModel  =
    let tetrominoWithPosition = gameModel.Tetromino
    let rotatedTetromino = rotation tetrominoWithPosition.Tetromino
    let newGameModel = { gameModel with Tetromino = { tetrominoWithPosition with Tetromino = rotatedTetromino} }
    checkMovement newGameModel.Grid newGameModel.Tetromino |> handleMovementOutcome newGameModel gameModel

let private handleMovement velocity (gameModel: GameModel) =
    let newGameModel = { gameModel with Tetromino = moveTetrominoByTetrominoVelocity gameModel.Tetromino velocity }
    checkMovement newGameModel.Grid newGameModel.Tetromino |> handleMovementOutcome newGameModel gameModel

let private handleInputRotateAntiClockwise = handleInputRotate rotateAntiClockwise

let private handleInputRotateClockwise = handleInputRotate rotateClockwise

let private handleInputMoveLeft = handleMovement (createTetrominoVelocity -1y 0y)

let private handleInputMoveRight = handleMovement (createTetrominoVelocity 1y 0y)

let reactToInput (stepState: CurrentStepState) (input: InputToGameModel) =
    let handleInput gameModel = match input with 
                                    | MoveLeft -> handleInputMoveLeft gameModel
                                    | MoveRight -> handleInputMoveRight gameModel
                                    | RotateClockwise -> handleInputRotateClockwise gameModel
                                    | RotateAntiClockwise -> handleInputRotateAntiClockwise gameModel
    bind handleInput stepState
    
let private placeTetromino gameModel =
    let placedGrid = { Grid = (addTetrominoToGrid gameModel.Grid gameModel.Tetromino); Tetromino = newTetromino()}
    { placedGrid with Grid = removeFilledRows placedGrid.Grid } 

let stepWorld (stepState: CurrentStepState) =
    let gameModel = stepState |> getGameModelFromStepState
    let newGameModel =  { gameModel with Tetromino = moveTetrominoByVelocity gameModel.Tetromino 0y -1y }
    let outcome = checkForPlacement newGameModel.Grid newGameModel.Tetromino 
    match outcome with
        | PlaceTetromino -> NotReactedToInput (placeTetromino gameModel)
        | DontPlaceTetromino -> NotReactedToInput newGameModel 
