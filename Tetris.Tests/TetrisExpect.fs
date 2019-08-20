module Tetris.Tests.TetrisExpect

open Expecto
open Expecto.Flip
open Tetris.GameModel.BlockColor
open Tetris.GameModel.Grid

let private convertToSimpleGrid grid =
    let getGrid(TetrisGrid grid) = grid
    let getRow(TetrisGridRow grid) = grid
    let grid = getGrid grid 
    let returnBoolRow (row: TetrisGridRow) = row |> getRow |> List.map(fun cell -> match cell with
                                                                                    | CellWithBlock _ -> 1
                                                                                    | _ -> 0)
    grid |> List.map(returnBoolRow) 

let shouldEqual expected actual =
    let actualReversed = actual |> convertToSimpleGrid
    let expectedReversed = expected |> List.rev
    actualReversed |> Expect.equal "The resultant grid should be expected to equal " expectedReversed

let blocksShouldAllHaveColor expected actual =
    let cellColors = actual |> getGridArray |> List.collect(getRowList) |> List.choose(getCellColor)
    cellColors |> Expect.all "All cells should have color" (fun color -> color = expected)
