module Tetris.GridCoordinates

type XCoordinate = private XCoordinate of sbyte with

    static member (+) (first: XCoordinate, second: XCoordinate) =
        let getValue (XCoordinate value) = value
        (getValue first) + (getValue second) |> XCoordinate

type YCoordinate = private YCoordinate of sbyte with
    
    static member (+) (first: YCoordinate, second: YCoordinate) =
        let getValue (YCoordinate value) = value
        (getValue first) + (getValue second) |> YCoordinate

type Coordinates = private {
    X: XCoordinate
    Y: YCoordinate
} with

    static member (+) (first: Coordinates, second: Coordinates) =
        { X = first.X + second.X; Y = first.Y + second.Y }
        
let createCoordinates x y =
    { X = XCoordinate x; Y = YCoordinate y }
