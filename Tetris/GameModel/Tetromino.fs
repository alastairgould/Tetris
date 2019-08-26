module Tetris.GameModel.Tetromino

open System
open Tetris.GameModel.BlockColor
open Tetris.GameModel.TetrominoShape
open Tetris.GameModel.GridCoordinates

type Tetromino =
    private
    | I of ColoredTetrominoShape
    | O of ColoredTetrominoShape
    | T of ColoredTetrominoShape
    | S of ColoredTetrominoShape
    | Z of ColoredTetrominoShape
    | J of ColoredTetrominoShape
    | L of ColoredTetrominoShape

type TetrominoPosition = private TetrominoPositionCoordinates of Coordinates

type TetrominoWithPosition = {
    Position: TetrominoPosition;
    Tetromino: Tetromino;
}

type TetrominoVelocity = private TetrominoMovement of Coordinates

let private randomGenerator = System.Random()

let createTetrominoPositionCoordinates x y = createCoordinates x y |> TetrominoPositionCoordinates

let createTetrominoVelocity x y = createCoordinates x y |> TetrominoMovement
        
let getTetrominoPositionCoordinates (TetrominoPositionCoordinates cords) = cords
    
let getTetrominoMovementCoordinates (TetrominoMovement cords) = cords

let moveTetrominoByTetrominoVelocity tetrominoWithPosition velocity = 
    let newPosition = (getTetrominoPositionCoordinates tetrominoWithPosition.Position) + (getTetrominoMovementCoordinates velocity) |> TetrominoPositionCoordinates
    {tetrominoWithPosition with Position = newPosition}

let moveTetrominoByVelocity tetrominoWithPosition dX dY = moveTetrominoByTetrominoVelocity tetrominoWithPosition (createTetrominoVelocity dX dY)

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

let tetrominoOverlaps tetrisGrid tetromino =
    let tetrominoPosition = getTetrominoPositionCoordinates tetromino.Position
    let shapeOnGridCoordinates = apply (translateColoredShapeToGridCoordinates tetrominoPosition) tetromino.Tetromino

    if isShapeOutsideOfBounds shapeOnGridCoordinates
        then true
    else
        doesColoredShapeOverlapWithBlocksOnGrid tetrisGrid shapeOnGridCoordinates
    
let addTetrominoToGrid tetrisGrid (tetromino: TetrominoWithPosition) =
    let tetrominoPosition = getTetrominoPositionCoordinates tetromino.Position
    let shapeOnGridCoordinates = apply (translateColoredShapeToGridCoordinates tetrominoPosition) tetromino.Tetromino
    addColoredShapeToGrid tetrisGrid shapeOnGridCoordinates
    
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

let rotateClockwise =
    map (fun shape -> shape |> rotateColoredShapeClockwise)

let rotateAntiClockwise =
    rotateClockwise >> rotateClockwise >> rotateClockwise
