module Tetris.GameModel.GridCoordinates

type YCoordinate = private YCoordinate of sbyte with
    static member (+) (first: YCoordinate, second: YCoordinate) =
        let getValue (YCoordinate value) = value
        (getValue first) + (getValue second) |> YCoordinate
    
type XCoordinate = private XCoordinate of sbyte with
    static member (+) (first: XCoordinate, second: XCoordinate) =
        let getValue (XCoordinate value) = value
        (getValue first) + (getValue second) |> XCoordinate

type Coordinates = private {
    X: XCoordinate
    Y: YCoordinate
} with
    static member (+) (first: Coordinates, second: Coordinates) =
        { X = first.X + second.X; Y = first.Y + second.Y }
        
let createCoordinates x y =
    { X = XCoordinate x; Y = YCoordinate y }

let rotateCoordinatesRight coordinates =
    let getXValue (XCoordinate value) = value
    let getYValue (YCoordinate value) = value
   
    let x = getXValue coordinates.X + 2y
    let y = getYValue coordinates.Y + 2y
    
    let newX = (y) - 2y
    let newY = (3y - x) - 2y
    
    {X = XCoordinate newX; Y = YCoordinate newY}
