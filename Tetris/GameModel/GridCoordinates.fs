module Tetris.GameModel.GridCoordinates

type YCoordinate = private YCoordinate of sbyte
    
type XCoordinate = private XCoordinate of sbyte 

type Coordinates = private {
    X: XCoordinate
    Y: YCoordinate
} 

let private getYValue(YCoordinate value) = value

let private getXValue (XCoordinate value) = value
    
let private toTuple coordinates = (getXValue coordinates.X), (getYValue coordinates.Y)

type YCoordinate with
    static member (+) (first: YCoordinate, second: YCoordinate) =
            (getYValue first) + (getYValue second) |> YCoordinate

type XCoordinate with
    static member (+) (first: XCoordinate, second: XCoordinate) =
            (getXValue first) + (getXValue second) |> XCoordinate 

type Coordinates with
    static member (+) (first: Coordinates, second: Coordinates) =
        { X = first.X + second.X; Y = first.Y + second.Y }

let createCoordinates x y = { X = XCoordinate x; Y = YCoordinate y }

let createCoordinatesWithIntegers x y = createCoordinates (sbyte x) (sbyte y)

let isCoordinatesOutOfBounds coordinates =
    let x, y = toTuple coordinates
    x < 0y || x > Grid.width - 1y || y < 0y || y > Grid.height - 1y

let findBoundingGridSizeForListOfCoords coords =
    let findHighest currentHighest nextValue = if nextValue > currentHighest
                                                then nextValue
                                                else currentHighest 
    
    let highestBound = coords |> List.collect (fun coords -> [coords.X |> getXValue;
                                                              coords.Y |> getYValue])
                              |> List.fold findHighest 0y 
                               
    highestBound + 1y
        
let rotateCoordinatesClockwise coordinates boundingGridSize =
    let oldX, oldY = toTuple coordinates
    
    let newX = oldY
    let newY = 1y - (oldX - (boundingGridSize - 2y))
    
    createCoordinates newX newY
