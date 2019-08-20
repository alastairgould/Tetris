module Tetris.GameModel.Collision

open Tetris.GameModel.Tetromino

type CollisionType =
    | CantTakeAction
    | TakeAction

type TetrominoPlacement =
    | PlaceTetromino
    | DontPlaceTetromino

let private overLappingBlocks tetromino grid =
    tetrominoOverlaps tetromino grid
    
let checkMovement tetromino grid = 
    let overLappingBlocks = overLappingBlocks tetromino grid
    match overLappingBlocks with
        | true -> CantTakeAction
        | false -> TakeAction

let checkForPlacement tetromino grid = 
    let overLappingBlocks = overLappingBlocks tetromino grid
    match overLappingBlocks with
        | true -> PlaceTetromino
        | false -> DontPlaceTetromino
