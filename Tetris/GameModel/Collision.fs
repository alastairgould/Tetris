module Tetris.GameModel.Collision

open Tetris.GameModel.Tetromino

type CollisionType =
    | CantTakeAction
    | TakeAction
    | TetrominoPlaced

type Direction =
    | Down
    | Left
    | Right
    
let overLappingBlocks tetromino grid =
    tetrominoOverlaps tetromino grid
    
let checkMovement tetromino grid direction = 
    let overLappingBlocks = overLappingBlocks tetromino grid
    match (direction, overLappingBlocks) with
        | (Left, true) -> CantTakeAction
        | (Left, false) -> TakeAction
        | (Right, true) -> CantTakeAction
        | (Right, false) -> TakeAction
        | (Down, true) -> TetrominoPlaced
        | (Down, false) -> TakeAction
