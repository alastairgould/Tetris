module Tetris.GameModel

open Tetris.Coordinates
open Tetris.Color
open Tetris.Tetromino

type TetrisGridCell =
    | CellWithBlock of Color
    | CellWithoutBlock

type TetrisGrid = TetrisGrid of (TetrisGridCell list) list
        
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

let private getBlockPlacementsForTetromino tetromino =
    let cords (BlockPlacementsForShape cords) = cords
    (tetromino |> getColoredShapeFromTetromino).Shape |> cords

let private translateBlockPlacement blockPlacement tetrominoPosition =
    blockPlacement + tetrominoPosition 
    
let private translateBlocks (tetrominoWithPosition: TetrominoWithPosition) =
    let blockPlacements = tetrominoWithPosition.Tetromino |> getBlockPlacementsForTetromino
    let position = tetrominoWithPosition.Position
    blockPlacements |> List.map (fun blockPlacement -> translateBlockPlacement blockPlacement position)
    
let private getColorOfTetromino tetromino =
    (tetromino |> getColoredShapeFromTetromino).Color
    
let private placeTetrisBlock tetromino coordinates =
    let translatedBlocks = translateBlocks tetromino
    let getCoordinatesFromBlockPlacement (BlockPlacementWithCoordinates coordinates) = coordinates
    let translatedCords = translatedBlocks |> List.map (getCoordinatesFromBlockPlacement)
    let containsBlock = translatedCords |> List.contains coordinates
    let color = getColorOfTetromino tetromino.Tetromino
    
    match containsBlock with
        | true -> CellWithBlock color
        | false -> CellWithoutBlock
    
let private addTetrominoToGrid tetrisGrid tetromino =
    let getGridArray (TetrisGrid grid) = grid
    let generateIndices size = [for x in 0y ..  sbyte (size - 1) -> x]
   
    let grid = getGridArray tetrisGrid
    let xIndexedGrid = grid |> List.map (fun row -> row |> List.zip (generateIndices row.Length))
    let yIndexedGrid = xIndexedGrid |> List.zip (generateIndices grid.Length) 
    
    let gridWithTetromino = yIndexedGrid |> List.map (fun row -> (snd row) |> List.map (fun cell ->
        let coordinates = createCoordinates (fst cell) (fst row)
        placeTetrisBlock tetromino coordinates)) 
    
    TetrisGrid gridWithTetromino
    
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
