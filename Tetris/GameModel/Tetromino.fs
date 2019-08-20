module Tetris.GameModel.Tetromino

open System
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

let private randomGenerator = System.Random()
        
let private createBlockPlacementCoordinates x y =
    createCoordinatesWithIntegers x y |> TetrominoBlock
    
let private getBlockPlacementCords (TetrominoBlock cords) = cords

let private getBlocksFromTetrominoShape (TetrominoShape shape) = shape

let private createShapeFromVisualArray (visualArray: int list list) =
    let blockPlacementForCell value x y = match (value) with
                                          | 1 -> Some (createBlockPlacementCoordinates x y)
                                          | _ -> None

    let blockPlacementForRow yIndex = List.mapi (fun xIndex cellValue -> blockPlacementForCell cellValue xIndex yIndex)
    let convertVisualTetrominoArrayToBlockPlacement2dOptionList = List.rev >> List.mapi (blockPlacementForRow)
    let convertOption2dListToFlatListWithoutNone = List.concat >> List.choose (fun element -> element)
    
    visualArray |> convertVisualTetrominoArrayToBlockPlacement2dOptionList
                |> convertOption2dListToFlatListWithoutNone
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

let private map f tetromino=
    match tetromino with
        | I coloredShape -> I (f coloredShape)
        | O coloredShape -> O (f coloredShape)
        | T coloredShape -> T (f coloredShape)
        | S coloredShape -> S (f coloredShape)
        | Z coloredShape -> Z (f coloredShape)
        | J coloredShape -> J (f coloredShape)
        | L coloredShape -> L (f coloredShape)

let private apply f tetromino=
    match tetromino with
        | I coloredShape -> f coloredShape
        | O coloredShape -> f coloredShape
        | T coloredShape -> f coloredShape
        | S coloredShape -> f coloredShape
        | Z coloredShape -> f coloredShape
        | J coloredShape -> f coloredShape
        | L coloredShape -> f coloredShape

let convertTetrominoWithPositionToTetrominoBlockList (tetromino: TetrominoWithPosition) =
    let translateColoredShapeToTetrominoBlocksOnGrid (position: TetrominoPosition) (coloredShape: ColoredTetrominoShape)  =
        let tetrominoBlocks = coloredShape.Shape |> getBlocksFromTetrominoShape
        tetrominoBlocks |> List.map(fun block -> block + position)
    
    let translateTetrominoToTetrominoBlocksOnGrid tetromino (position: TetrominoPosition) =
        let translateColoredShapeToTetrominoBlocksOnGridWithPosition = translateColoredShapeToTetrominoBlocksOnGrid position
        apply translateColoredShapeToTetrominoBlocksOnGridWithPosition tetromino

    translateTetrominoToTetrominoBlocksOnGrid tetromino.Tetromino tetromino.Position

let private shouldCellContainBlock translatedBlocks coordinates =
    let getCoordinatesFromBlockPlacement (TetrominoBlock coordinates) = coordinates
    let translatedCoords = translatedBlocks |> List.map(getCoordinatesFromBlockPlacement)
    translatedCoords |> List.contains coordinates
   
let private createCellForCoordinates color translatedBlocks coordinates =
    match (shouldCellContainBlock translatedBlocks coordinates) with
        | true -> CellWithBlock color
        | false -> CellWithoutBlock
  
let createTetrominoVelocity x y =
    createCoordinates x y |> TetrominoMovement

let createTetrominoPositionCoordinates x y =
    createCoordinates x y |> TetrominoPositionCoordinates
   
let tetrominoOverlaps tetrisGrid tetromino =
    let translatedBlocks = convertTetrominoWithPositionToTetrominoBlockList tetromino
    
    let areAnyTranslatedBlocksOutOfBounds = List.map(getBlockPlacementCords)
                                            >> List.map(isCoordinatesOutOfBounds)
                                            >> List.contains(true)
    
    let overlappingBlock (currentCell: TetrisGridCell) translatedBlocks coordinates =
       let shouldContainBlock = shouldCellContainBlock translatedBlocks coordinates 
       let currentlyContainsBlock = match currentCell with
                                    | CellWithBlock _ -> true
                                    | CellWithoutBlock _ -> false
       shouldContainBlock && currentlyContainsBlock
    
    let rowWithTetromino yIndex = List.mapi (fun xIndex cell ->
        createCoordinatesWithIntegers xIndex yIndex |> overlappingBlock cell translatedBlocks)
    
    let tetrisRowWithTetromino yIndex = getRowList >> rowWithTetromino yIndex
    
    let outOfBounds = areAnyTranslatedBlocksOutOfBounds translatedBlocks
    
    if outOfBounds
        then true
    else
        tetrisGrid
                |> getGridArray
                |> List.mapi(tetrisRowWithTetromino)
                |> List.concat
                |> List.contains(true)
    
let addTetrominoToGrid tetrisGrid tetromino =
    let translatedBlocks = convertTetrominoWithPositionToTetrominoBlockList tetromino
    let color = apply (fun shape -> shape.Color) tetromino.Tetromino
    
    let rowWithTetromino yIndex = List.mapi (fun xIndex cell ->
        match cell with
            | CellWithBlock _c -> cell
            | CellWithoutBlock -> createCoordinatesWithIntegers xIndex yIndex |> createCellForCoordinates color translatedBlocks)
    
    let tetrisRowWithTetromino yIndex = getRowList >> rowWithTetromino yIndex >> TetrisGridRow

    tetrisGrid
        |> getGridArray
        |> List.mapi(tetrisRowWithTetromino)
        |> TetrisGrid
    
let createTetromino() =
    let randomNumber = randomGenerator.Next(0, 7)
    
    match randomNumber with
        | 0 -> createITetromino
        | 1 -> createJTetromino
        | 2 -> createLTetromino
        | 3 -> createOTetromino
        | 4 -> createSTetromino
        | 5 -> createTTetromino
        | 6 -> createZTetromino
        | _ -> raise(InvalidOperationException("Random number generated a number out of expected range"))
    
let private rotateTetrominoShapeClockwise tetrominoShape = 
    let blockCoords = tetrominoShape |> getBlocksFromTetrominoShape
                                     |> List.map(fun blockCoordinate -> getBlockPlacementCords blockCoordinate)
    
    let highestBound = findBoundingGridSizeForListOfCoords blockCoords
    
    blockCoords |> List.map(fun blockCords -> rotateCoordinatesClockwise blockCords highestBound)
                |> List.map(fun rotatedCords -> TetrominoBlock rotatedCords)
                |> TetrominoShape
 
let rotateClockwise =
    map (fun shape -> {shape with Shape = shape.Shape |> rotateTetrominoShapeClockwise })

let rotateAntiClockwise =
    rotateClockwise >> rotateClockwise >> rotateClockwise
