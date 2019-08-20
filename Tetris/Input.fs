module Tetris.Input

open Tetris.GameModel.GameModel
open SFML.Window

type Input =
    | GameModelInput of InputToGameModel
    | Quit

let getProgramInput() =
    if Keyboard.IsKeyPressed Keyboard.Key.Right then
        Some (GameModelInput MoveRight)
    elif Keyboard.IsKeyPressed Keyboard.Key.Left then
        Some (GameModelInput MoveLeft)
    elif Keyboard.IsKeyPressed Keyboard.Key.LAlt then
        Some (GameModelInput RotateAntiClockwise)
    elif Keyboard.IsKeyPressed Keyboard.Key.LControl then
        Some (GameModelInput RotateClockwise)
    elif Keyboard.IsKeyPressed Keyboard.Key.Escape then
        Some Quit
    else
        None
