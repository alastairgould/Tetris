module Tetris.Tetromino

open Tetris.Coordinates
open Tetris.Color

type BlockPlacementWithCoordinates = BlockPlacementWithCoordinates of Coordinates

type BlockPlacementsForShape = BlockPlacementsForShape of BlockPlacementWithCoordinates list

type ColoredShape = {
    Shape: BlockPlacementsForShape
    Color: Color
}

type Tetromino = 
    private
    | I of ColoredShape
    | O of ColoredShape
    | T of ColoredShape
    | S of ColoredShape
    | Z of ColoredShape
    | J of ColoredShape
    | L of ColoredShape

type TetrominoPositionCoordinates = TetrominoPositionCoordinates of Coordinates with
    
    static member (+) (first: BlockPlacementWithCoordinates, second: TetrominoPositionCoordinates) =
        let getBlockPlacementCords (BlockPlacementWithCoordinates cords) = cords
        let getTetrominoPositionCords (TetrominoPositionCoordinates cords) = cords
        (getBlockPlacementCords first) + (getTetrominoPositionCords second) |> BlockPlacementWithCoordinates

type TetrominoWithPosition = {
    Position: TetrominoPositionCoordinates;
    Tetromino: Tetromino;
}

type TetrominoMovement = TetrominoMovement of Coordinates with
    
    static member (+) (first: TetrominoWithPosition, second: TetrominoMovement) =
        let getTetrominoMovementCords (TetrominoMovement cords) = cords
        let getTetrominoPositionCords (TetrominoPositionCoordinates cords) = cords
        let newPosition = (getTetrominoPositionCords first.Position) + (getTetrominoMovementCords second) |> TetrominoPositionCoordinates
        {first with Position = newPosition}
        
let private createBlockPlacementCoordinates x y =
    createCoordinates x y |> BlockPlacementWithCoordinates

let createTetrominoPositionCoordinates x y =
    createCoordinates x y |> TetrominoPositionCoordinates

let createTetrominoMovement x y =
    createCoordinates x y |> TetrominoMovement

let private createITetromino =
    let color = Orange 
    let blockPlacements = BlockPlacementsForShape [ for y in 0y .. 3y -> createBlockPlacementCoordinates 0y y ]
    let coloredShape = { Shape = blockPlacements; Color = color }
    I coloredShape

let private createOTetromino =
    let blockPlacements = BlockPlacementsForShape ([for y in 0y .. 1y -> [for x in 0y .. 1y -> createBlockPlacementCoordinates x y]] |> List.concat) 
    let color = Yellow
    let coloredShape = { Shape = blockPlacements; Color = color }
    O coloredShape
    
let getColoredShapeFromTetromino tetromino =
    match tetromino with
        | I coloredShape -> coloredShape
        | O coloredShape -> coloredShape
        | T coloredShape -> coloredShape
        | S coloredShape -> coloredShape
        | Z coloredShape -> coloredShape
        | J coloredShape -> coloredShape
        | L coloredShape -> coloredShape
    
let createTetromino =
    createITetromino