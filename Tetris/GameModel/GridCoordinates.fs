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

let findBoundingGridSizeForListOfCoords coords =
    let getYValue (YCoordinate value) = value
    let getXValue (XCoordinate value) = value
    
    let findHighest currentHighest nextValue = if nextValue > currentHighest
                                                then nextValue
                                                else currentHighest 
    
    let highestBound = coords |> List.collect (fun coords -> [coords.X |> getXValue;
                                                              coords.Y |> getYValue])
                              |> List.fold findHighest 0y 
                               
    highestBound + 1y
        
let rotateCoordinatesRight coordinates boundingGridSize =
    let getXValue (XCoordinate value) = value
    let getYValue (YCoordinate value) = value
   
    let oldX = getXValue coordinates.X 
    let oldY = getYValue coordinates.Y
    
    let newX = oldY
    let newY = 1y - (oldX - (boundingGridSize - 2y))
    
    createCoordinates newX newY
