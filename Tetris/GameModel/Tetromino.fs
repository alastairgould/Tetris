module Tetris.GameModel.Tetromino

open Tetris.GameModel
open Tetris.GameModel.BlockColor
open Tetris.GameModel.GridCoordinates
open Tetris.GameModel.Grid

type BlockPlacementWithCoordinates = private BlockPlacementWithCoordinates of Coordinates

type BlockPlacementsForShape = private BlockPlacementsForShape of BlockPlacementWithCoordinates list

type ColoredShape = private {
    Shape: BlockPlacementsForShape
    Color: BlockColor
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

type TetrominoPositionCoordinates = private TetrominoPositionCoordinates of Coordinates with
    
    static member (+) (first: BlockPlacementWithCoordinates, second: TetrominoPositionCoordinates) =
        let getBlockPlacementCords (BlockPlacementWithCoordinates cords) = cords
        let getTetrominoPositionCords (TetrominoPositionCoordinates cords) = cords
        (getBlockPlacementCords first) + (getTetrominoPositionCords second) |> BlockPlacementWithCoordinates

type TetrominoWithPosition = {
    Position: TetrominoPositionCoordinates;
    Tetromino: Tetromino;
}

type TetrominoMovement = private TetrominoMovement of Coordinates with
    
    static member (+) (first: TetrominoWithPosition, second: TetrominoMovement) =
        let getTetrominoMovementCords (TetrominoMovement cords) = cords
        let getTetrominoPositionCords (TetrominoPositionCoordinates cords) = cords
        let newPosition = (getTetrominoPositionCords first.Position) + (getTetrominoMovementCords second) |> TetrominoPositionCoordinates
        {first with Position = newPosition}
        
let private createBlockPlacementCoordinates x y =
    createCoordinates x y |> BlockPlacementWithCoordinates

let createITetromino =
    let color = Orange 
    let blockPlacements = BlockPlacementsForShape [ for x in 0y .. 3y -> createBlockPlacementCoordinates x 2y ]
    let coloredShape = { Shape = blockPlacements; Color = color }
    I coloredShape

let createOTetromino =
    let blockPlacements = BlockPlacementsForShape ([for y in 0y .. 1y -> [for x in 0y .. 1y -> createBlockPlacementCoordinates x y]] |> List.concat) 
    let color = Orange
    let coloredShape = { Shape = blockPlacements; Color = color }
    O coloredShape

let createJTetromino =
    let blockPlacements = BlockPlacementsForShape ([for y in 1y .. 2y -> match y with
                                                                            | y when y = 1y -> [for x in 0y .. 2y -> createBlockPlacementCoordinates x y]
                                                                            | y when y = 2y -> [createBlockPlacementCoordinates 0y y]
                                                   ] |> List.concat) 
    let color = Orange
    let coloredShape = { Shape = blockPlacements; Color = color }
    O coloredShape

let private getColoredShapeFromTetromino tetromino =
    match tetromino with
        | I coloredShape -> coloredShape
        | O coloredShape -> coloredShape
        | T coloredShape -> coloredShape
        | S coloredShape -> coloredShape
        | Z coloredShape -> coloredShape
        | J coloredShape -> coloredShape
        | L coloredShape -> coloredShape

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
    
let private shouldCellContainBlock tetromino coordinates =
    let translatedBlocks = translateBlocks tetromino
    let getCoordinatesFromBlockPlacement (BlockPlacementWithCoordinates coordinates) = coordinates
    let translatedCords = translatedBlocks |> List.map (getCoordinatesFromBlockPlacement)
    let containsBlock = translatedCords |> List.contains coordinates
    let color = getColorOfTetromino tetromino.Tetromino
    
    match containsBlock with
        | true -> CellWithBlock color
        | false -> CellWithoutBlock

let createTetrominoMovement x y =
    createCoordinates x y |> TetrominoMovement

let createTetrominoPositionCoordinates x y =
    createCoordinates x y |> TetrominoPositionCoordinates
    
let addTetrominoToGrid tetrisGrid tetromino =
    let getGridArray (TetrisGrid grid) = grid
    let generateIndices size = [for x in 0y ..  sbyte (size - 1) -> x]
   
    let grid = getGridArray tetrisGrid
    let xIndexedGrid = grid |> List.map (fun row -> row |> List.zip (generateIndices row.Length))
    let yIndexedGrid = xIndexedGrid |> List.zip (generateIndices grid.Length) 
    
    let gridWithTetromino = yIndexedGrid |> List.map (fun row -> (snd row) |> List.map (fun cell ->
        let coordinates = createCoordinates (fst cell) (fst row)
        shouldCellContainBlock tetromino coordinates)) 
    
    TetrisGrid gridWithTetromino
    
let createTetromino =
    createITetromino
   

   
let rotateTetrominoRight tetromino =
    let getBlockPlacementCords (BlockPlacementWithCoordinates cords) = cords
    let tetrominoBlocks = getBlockPlacementsForTetromino tetromino
    let coloredShape = getColoredShapeFromTetromino tetromino
    
    let blockCoords = tetrominoBlocks |> List.map(fun blockCoordinate -> getBlockPlacementCords blockCoordinate)
    let highestBound = findBoundGridSize blockCoords                         
                                       
    let newBlockCords = blockCoords |> List.map(fun blockCords -> rotateCoordinatesRight blockCords highestBound)
                                             |> List.map(fun rotatedCords -> BlockPlacementWithCoordinates rotatedCords)
                                             |> BlockPlacementsForShape
                                        
    let newColoredShape = {coloredShape with Shape = newBlockCords}
    I newColoredShape
