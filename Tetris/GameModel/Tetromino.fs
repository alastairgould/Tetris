module Tetris.GameModel.Tetromino

open Tetris.GameModel.BlockColor
open Tetris.GameModel.GridCoordinates
open Tetris.GameModel.Grid

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
        
let private generateIndices size = [for x in 0y ..  sbyte (size - 1) -> x]

let private createBlockPlacementCoordinates x y =
    createCoordinates x y |> TetrominoBlock

let private createShapeFromVisualArray (list: int list list) =
    let indexedX = list |> List.map(fun row -> row |> List.zip (generateIndices row.Length))
    let indexedY = indexedX |> List.rev |> List.zip (generateIndices list.Length)
    
    let isBlockPlacement value = if value > 0
                                    then true
                                    else false
    
    let blockPlacement x y value = if (isBlockPlacement value)
                                        then Some (createBlockPlacementCoordinates x y)
                                        else None
     
    let blockPlacements = indexedY |> List.collect (fun y -> (snd y) |> List.choose(fun x -> (blockPlacement (fst x) (fst y) (snd x))))
    blockPlacements |> TetrominoShape

let createITetromino =
    let shape = [[0; 0; 0; 0];
                 [1; 1; 1; 1];
                 [0; 0; 0; 0];
                 [0; 0; 0; 0]] |> createShapeFromVisualArray

    let color = LightBlue 
    let coloredShape = { Shape = shape; Color = color }
    I coloredShape

let createOTetromino =
    let shape = [[1; 1];
                 [1; 1]] |> createShapeFromVisualArray

    let color = Yellow
    let coloredShape = { Shape = shape; Color = color }
    O coloredShape

let createJTetromino =
    let shape = [[1; 0; 0];
                 [1; 1; 1];
                 [0; 0; 0]] |> createShapeFromVisualArray

    let color = DarkBlue
    let coloredShape = { Shape = shape; Color = color }
    J coloredShape

let createLTetromino =
    let shape = [[0; 0; 1];
                 [1; 1; 1];
                 [0; 0; 0]] |> createShapeFromVisualArray
    
    let color = Orange
    let coloredShape = { Shape = shape; Color = color }
    L coloredShape

let createSTetromino =
    let shape = [[0; 1; 1];
                 [1; 1; 0];
                 [0; 0; 0]] |> createShapeFromVisualArray
    
    let color = Green
    let coloredShape = { Shape = shape; Color = color }
    S coloredShape

let createTTetromino =
    let shape = [[0; 1; 0];
                 [1; 1; 1];
                 [0; 0; 0]] |> createShapeFromVisualArray
    
    let color = Purple
    let coloredShape = { Shape = shape; Color = color }
    T coloredShape

let createZTetromino =
    let shape = [[1; 1; 0];
                 [0; 1; 1];
                 [0; 0; 0]] |> createShapeFromVisualArray
    
    let color = Red
    let coloredShape = { Shape = shape; Color = color }
    Z coloredShape

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

let createTetrominoMovement x y =
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
    createZTetromino
   
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
