module Tetris.GameModel.TetrominoShape

open Tetris.GameModel.Grid
open Tetris.GameModel.BlockColor
open Tetris.GameModel.GridCoordinates

type TetrominoBlock = private TetrominoBlock of Coordinates

type TetrominoShape = private TetrominoShape of TetrominoBlock list

type ColoredTetrominoShape = private {
    Shape: TetrominoShape
    Color: BlockColor
}

let private createBlockPlacementCoordinates x y = createCoordinatesWithIntegers x y |> TetrominoBlock
    
let private getBlockPlacementCoordinates (TetrominoBlock cords) = cords

let private getBlocksFromTetrominoShape (TetrominoShape shape) = shape

let private getShapeFromColoredShape coloredShape = coloredShape.Shape
   
let private createShapeFromBlockPlacementCoordinates = List.map(TetrominoBlock) >> TetrominoShape

let private getBlockPlacementCoordinatesFromShape = getBlocksFromTetrominoShape >> List.map getBlockPlacementCoordinates
    
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

let private createColoredShape color shape = { Shape = shape; Color = color }
    
let private translateShapeToGridCoordinates gridCoordinate = getBlockPlacementCoordinatesFromShape
                                                             >> List.map(fun blockCoordinate -> blockCoordinate + gridCoordinate)
                                                             >> createShapeFromBlockPlacementCoordinates

let translateColoredShapeToGridCoordinates gridCoordinate coloredShape =
    { coloredShape with Shape = translateShapeToGridCoordinates gridCoordinate coloredShape.Shape }

let private shouldCellContainBlock translatedBlocks coordinates =
    let getCoordinatesFromBlockPlacement (TetrominoBlock coordinates) = coordinates
    let translatedCoords = translatedBlocks |> List.map(getCoordinatesFromBlockPlacement)
    translatedCoords |> List.contains coordinates

let private createCellForCoordinates color translatedBlocks coordinates =
    match (shouldCellContainBlock translatedBlocks coordinates) with
        | true -> CellWithBlock color
        | false -> CellWithoutBlock
  
let addColoredShapeToGrid tetrisGrid coloredShape =
    let shapeBlocks = coloredShape.Shape |> getBlocksFromTetrominoShape
    
    let rowWithTetromino yIndex = List.mapi (fun xIndex cell ->
        match cell with
            | CellWithBlock _c -> cell
            | CellWithoutBlock -> createCoordinatesWithIntegers xIndex yIndex |> createCellForCoordinates coloredShape.Color shapeBlocks)
    
    let tetrisRowWithTetromino yIndex = getRowList >> rowWithTetromino yIndex >> TetrisGridRow
    
    tetrisGrid |> getGridArray
               |> List.mapi(tetrisRowWithTetromino)
               |> TetrisGrid

let private doesShapeOverlapWithBlocksOnGrid tetrisGrid shape = 
    let overlappingBlock (currentCell: TetrisGridCell) translatedBlocks coordinates =
       let shouldContainBlock = shouldCellContainBlock translatedBlocks coordinates 
       let currentlyContainsBlock = match currentCell with
                                    | CellWithBlock _ -> true
                                    | CellWithoutBlock _ -> false
       shouldContainBlock && currentlyContainsBlock
    
    let rowWithTetromino yIndex = List.mapi (fun xIndex cell ->
        createCoordinatesWithIntegers xIndex yIndex |> overlappingBlock cell (shape |> getBlocksFromTetrominoShape))
    
    let tetrisRowWithTetromino yIndex = getRowList >> rowWithTetromino yIndex
    
    tetrisGrid |> getGridArray
               |> List.mapi(tetrisRowWithTetromino)
               |> List.concat
               |> List.contains(true)
     
let doesColoredShapeOverlapWithBlocksOnGrid tetrisGrid coloredShape =
    doesShapeOverlapWithBlocksOnGrid tetrisGrid coloredShape.Shape
     
let createShapeFromVisualArrayWithColor color = createShapeFromVisualArray >> createColoredShape color

let private rotateShapeClockwise tetrominoShape = 
    let blockCoords = tetrominoShape |> getBlockPlacementCoordinatesFromShape
    let highestBound = findBoundingGridSizeForListOfCoords blockCoords
    
    blockCoords |> List.map(fun blockCords -> rotateCoordinatesClockwise blockCords highestBound)
                |> createShapeFromBlockPlacementCoordinates
                
let rotateColoredShapeClockwise (coloredTetrominoShape: ColoredTetrominoShape) =
    { coloredTetrominoShape with Shape = rotateShapeClockwise coloredTetrominoShape.Shape}

let isShapeOutsideOfBounds = getShapeFromColoredShape >> getBlockPlacementCoordinatesFromShape
                                                      >> List.map isCoordinatesOutOfBounds
                                                      >> List.contains true