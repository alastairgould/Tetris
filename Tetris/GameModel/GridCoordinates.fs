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

let findBoundGridSize coords =
    let getYValue (YCoordinate value) = value
    let getXValue (XCoordinate value) = value
    
    let listOfYs = coords |> List.map (fun coords -> coords.Y)
    let listOfXs = coords |> List.map (fun coords -> coords.X)
   
    let findHighest currentHighest nextValue = if nextValue > currentHighest
                                                    then nextValue
                                                    else currentHighest 
     
    let highestY = listOfYs |> List.fold findHighest (YCoordinate 0y) |> getYValue
    let highestX = listOfXs |> List.fold findHighest (XCoordinate 0y) |> getXValue
   
    let highestBound = if highestX > highestY
                        then highestX
                        else highestY
    
    highestBound + 1y
        
let rotateCoordinatesRight coordinates size =
    let getXValue (XCoordinate value) = value
    let getYValue (YCoordinate value) = value
   
    let x = getXValue coordinates.X 
    let y = getYValue coordinates.Y
    
    let newX = y
    let newY = 1y - (x - (size - 2y))
    
    {X = XCoordinate newX; Y = YCoordinate newY}
