module Tetris.GameModel.Tetromino

open Tetris.GameModel.BlockColor
open Tetris.GameModel.Grid
open Tetris.GameModel.GridCoordinates

type TetrominoBlock = private TetrominoBlock of Coordinates

type TetrominoShape = private TetrominoShape of TetrominoBlock list

type ColoredTetrominoShape = private {
    Shape: TetrominoShape
    Color: BlockColor
}

type Tetromino =
    private
    | I of ColoredTetrominoShape
    | O of ColoredTetrominoShape
    | T of ColoredTetrominoShape
    | S of ColoredTetrominoShape
    | Z of ColoredTetrominoShape
    | J of ColoredTetrominoShape
    | L of ColoredTetrominoShape

type TetrominoPosition = private TetrominoPositionCoordinates of Coordinates with
    static member (+) (first: TetrominoBlock, second: TetrominoPosition) =
        let getBlockPlacementCords (TetrominoBlock cords) = cords
        let getTetrominoPositionCords (TetrominoPositionCoordinates cords) = cords
        (getBlockPlacementCords first) + (getTetrominoPositionCords second) |> TetrominoBlock

type TetrominoWithPosition = {
    Position: TetrominoPosition;
    Tetromino: Tetromino;
}

type TetrominoVelocity = private TetrominoMovement of Coordinates with
    static member (+) (first: TetrominoWithPosition, second: TetrominoVelocity) =
        let getTetrominoMovementCords (TetrominoMovement cords) = cords
        let getTetrominoPositionCords (TetrominoPositionCoordinates cords) = cords
        let newPosition = (getTetrominoPositionCords first.Position) + (getTetrominoMovementCords second) |> TetrominoPositionCoordinates
        {first with Position = newPosition}
        
let private createBlockPlacementCoordinates x y =
    createCoordinates (sbyte x) (sbyte y) |> TetrominoBlock

let private createShapeFromVisualArray (visualArray: int list list) =
    let blockPlacementForCell value x y = match (value) with
                                          | 1 -> Some (createBlockPlacementCoordinates x y)
                                          | _ -> None

    let blockPlacementsForRow yIndex = List.mapi(fun xIndex cellValue -> blockPlacementForCell cellValue xIndex yIndex)
    let convertToBlockPlacement2dOptionList = List.rev >> List.mapi(blockPlacementsForRow)
    let convertOption2dListToFlatList = List.concat >> List.choose(fun element -> element)
    
    visualArray |> convertToBlockPlacement2dOptionList
                |> convertOption2dListToFlatList
                |> TetrominoShape

let private createColoredShape color shape =
    { Shape = shape; Color = color }
    
let private createShapeFromVisualArrayWithColor color = createShapeFromVisualArray >> createColoredShape color

let createITetromino =
    let shape = [[0; 0; 0; 0];
                 [1; 1; 1; 1];
                 [0; 0; 0; 0];
                 [0; 0; 0; 0]] |> createShapeFromVisualArrayWithColor LightBlue
    I shape

let createOTetromino =
    let shape = [[1; 1];
                 [1; 1]] |> createShapeFromVisualArrayWithColor Yellow
    O shape

let createJTetromino =
    let shape = [[1; 0; 0];
                 [1; 1; 1];
                 [0; 0; 0]] |> createShapeFromVisualArrayWithColor DarkBlue
    J shape

let createLTetromino =
    let shape = [[0; 0; 1];
                 [1; 1; 1];
                 [0; 0; 0]] |> createShapeFromVisualArrayWithColor Orange
    L shape

let createSTetromino =
    let shape = [[0; 1; 1];
                 [1; 1; 0];
                 [0; 0; 0]] |> createShapeFromVisualArrayWithColor Green
    S shape

let createTTetromino =
    let shape = [[0; 1; 0];
                 [1; 1; 1];
                 [0; 0; 0]] |> createShapeFromVisualArrayWithColor Purple
    T shape

let createZTetromino =
    let shape = [[1; 1; 0];
                 [0; 1; 1];
                 [0; 0; 0]] |> createShapeFromVisualArrayWithColor Red
    Z shape

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
    let cords (TetrominoShape cords) = cords
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
    let getCoordinatesFromBlockPlacement (TetrominoBlock coordinates) = coordinates
    let translatedCords = translatedBlocks |> List.map (getCoordinatesFromBlockPlacement)
    let containsBlock = translatedCords |> List.contains coordinates
    let color = getColorOfTetromino tetromino.Tetromino
    
    match containsBlock with
        | true -> CellWithBlock color
        | false -> CellWithoutBlock

let createTetrominoVelocity x y =
    createCoordinates x y |> TetrominoMovement

let createTetrominoPositionCoordinates x y =
    createCoordinates x y |> TetrominoPositionCoordinates
    
let addTetrominoToGrid tetrisGrid tetromino =
    let getGridArray (TetrisGrid grid) = grid
    let getRowList (TetrisGridRow grid) = grid
   
    let grid = getGridArray tetrisGrid
    let xIndexedGrid = grid |> List.map (fun row -> row |> getRowList
                                                        |> List.zip (generateIndices (getRowList row).Length))
    let yIndexedGrid = xIndexedGrid |> List.zip (generateIndices grid.Length) 
    
    let gridWithTetromino = yIndexedGrid |> List.map (fun row -> TetrisGridRow ((snd row) |> List.map (fun cell ->
        let coordinates = createCoordinates (fst cell) (fst row)
        shouldCellContainBlock tetromino coordinates)))
    
    TetrisGrid gridWithTetromino
    
let createTetromino =
    createTTetromino
   
let rotateClockwise tetromino =
    let getBlockPlacementCords (TetrominoBlock cords) = cords
    let tetrominoBlocks = getBlockPlacementsForTetromino tetromino
    let coloredShape = getColoredShapeFromTetromino tetromino
    
    let blockCoords = tetrominoBlocks |> List.map(fun blockCoordinate -> getBlockPlacementCords blockCoordinate)
    let highestBound = findBoundingGridSizeForListOfCoords blockCoords                         
                                       
    let newBlockCords = blockCoords |> List.map(fun blockCords -> rotateCoordinatesRight blockCords highestBound)
                                    |> List.map(fun rotatedCords -> TetrominoBlock rotatedCords)
                                    |> TetrominoShape
                                        
    let newColoredShape = {coloredShape with Shape = newBlockCords}
    I newColoredShape
