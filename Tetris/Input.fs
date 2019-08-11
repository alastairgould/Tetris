module Tetris.Input

open Tetris.GameModel
open SFML.Window

type Input =
    | GameModelInput of InputToGameModel
    | Quit

let getProgramInput() =
    if Keyboard.IsKeyPressed Keyboard.Key.Right then
        Some (GameModelInput MoveRight)
    elif Keyboard.IsKeyPressed Keyboard.Key.Left then
        Some (GameModelInput MoveLeft)
    elif Keyboard.IsKeyPressed Keyboard.Key.Escape then
        Some Quit
    else
        None

