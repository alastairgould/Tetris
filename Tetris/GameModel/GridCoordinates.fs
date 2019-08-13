module Tetris.GameModel.GridCoordinates

type YCoordinate = private YCoordinate of sbyte with
    static member (+) (first: YCoordinate, second: YCoordinate) =
        let getValue (YCoordinate value) = value
        (getValue first) + (getValue second) |> YCoordinate
    
    static member (*) (first: YCoordinate, second: YCoordinate) =
        let getValue (YCoordinate value) = value
        (getValue first) * (getValue second) |> YCoordinate

type XCoordinate = private XCoordinate of sbyte with
    static member (+) (first: XCoordinate, second: XCoordinate) =
        let getValue (XCoordinate value) = value
        (getValue first) + (getValue second) |> XCoordinate
    
    static member (+) (first: XCoordinate, second: YCoordinate) =
        let getXValue (XCoordinate value) = value
        let getYValue (YCoordinate value) = value
        (getXValue first) + (getYValue second) |> XCoordinate
    
    static member (+) (first: YCoordinate, second: XCoordinate) =
        let getXValue (XCoordinate value) = value
        let getYValue (YCoordinate value) = value
        (getYValue first) + (getXValue second) |> YCoordinate
    
    static member (*) (first: XCoordinate, second: XCoordinate) =
        let getValue (XCoordinate value) = value
        (getValue first) * (getValue second) |> XCoordinate

type Coordinates = private {
    X: XCoordinate
    Y: YCoordinate
} with
    static member (+) (first: Coordinates, second: Coordinates) =
        { X = first.X + second.X; Y = first.Y + second.Y }
        
type RotationRow = private RotationRow of sbyte list
    
type RotationMatrix = private RotationMatrix of RotationRow list
    with
        static member (*) (coordinates: Coordinates, matrix: RotationMatrix) =
            let rowList (RotationRow value) = value
            let matrixList (RotationMatrix value) = value
            let matrixRowList = matrixList matrix
            
            let firstRow = rowList matrixRowList.[0] 
            let secondRow = rowList matrixRowList.[1] 
            
            let x = coordinates.X * XCoordinate firstRow.[0] + coordinates.Y * YCoordinate firstRow.[1]
            let y = coordinates.Y * YCoordinate secondRow.[1] + coordinates.X * XCoordinate secondRow.[0] 
            
            { X = x; Y = y }

let private rightRotationMatrix =
    RotationMatrix [RotationRow [0y; -1y];
                    RotationRow [1y; 0y]]

let createCoordinates x y =
    { X = XCoordinate x; Y = YCoordinate y }

let rotateCoordinatesRight coordinates =
    coordinates * rightRotationMatrix
