module Tetris.Renderer

open Tetris.GameModel.BlockColor
open Tetris.GameModel.Grid
open SFML.Graphics
open SFML.System

type MouseDraggingContext = {
    X: int
    Y: int
} with
    static member (-) (first: MouseDraggingContext, second: MouseDraggingContext) =
        { X = first.X - second.X; Y = first.Y - second.Y }

type MouseDragging =
    | MouseDragging of MouseDraggingContext
    | MouseNotDragging

type SFMLRenderContext = {
    RenderWindow: RenderWindow
    MouseDragging : MouseDragging
}

type Renderer = SFMLRenderer of SFMLRenderContext

type private RenderableBlock = {
    xSize: int32
    ySize: int32
    xPos: int32
    yPos: int32
    Color: BlockColor option
}

let private convertFromColorToSfmlColor (color: BlockColor) =
    match color with
    | LightBlue -> SFML.Graphics.Color.Cyan
    | DarkBlue -> SFML.Graphics.Color.Blue
    | Orange -> SFML.Graphics.Color.Magenta
    | Yellow -> SFML.Graphics.Color.Yellow
    | Green -> SFML.Graphics.Color.Green
    | Purple -> SFML.Graphics.Color.Red
    | Red -> SFML.Graphics.Color.Red

let private convertFromOptionColorToSfmlColor (color: BlockColor option) = 
    match color with
    | Some color ->  convertFromColorToSfmlColor color
    | None -> SFML.Graphics.Color(240uy, 240uy, 240uy)

let private renderGrid (window: RenderWindow) grid =
    let getGridArray (TetrisGrid grid) = grid

    let yCords = [for y in 0 .. ((getGridArray grid).Length) - 1 -> y] |> List.rev
    let generateXIndexes size = [for x in 0 .. size -> x]

    let xIndexedGrid = (getGridArray grid) |> List.map (fun row -> row |> List.zip (generateXIndexes (row.Length - 1)))
    let yIndexedGrid = xIndexedGrid |> List.zip yCords

    let renderableBlockFromCell xPos yPos cell = 
        match cell with
        | CellWithoutBlock -> {xSize = 30; ySize = 30; xPos = 30 * (xPos); yPos = 30 * yPos; Color = None; }
        | CellWithBlock color -> {xSize = 30; ySize = 30; xPos = 30 * (xPos); yPos = 30 * yPos; Color = Some color; }

    let renderableBlocks = yIndexedGrid |> List.collect (fun row -> (snd row) |> List.map (fun cell -> renderableBlockFromCell (fst cell) (fst row) (snd cell)))

    let rectShapes = renderableBlocks |> List.map (fun block -> 
        let vector = Vector2f((float32 block.xSize), (float32 block.ySize))
        let rectShape = new RectangleShape(vector)

        rectShape.Position <- Vector2f((float32 block.xPos + 15.0f), (float32 block.yPos + 15.0f))
        rectShape.FillColor <- convertFromOptionColorToSfmlColor block.Color
        rectShape.Origin <- Vector2f(15.0f, 15.0f)
        rectShape.Scale <- Vector2f(0.8f, 0.8f)
        rectShape)

    rectShapes |> List.iter (window.Draw)
    rectShapes |> List.iter (fun sg -> sg.Dispose())
    ()

let private getMouseContext() =
    let mousePosition = SFML.Window.Mouse.GetPosition()
    { X = mousePosition.X; Y = mousePosition.Y } 

let private moveWindow oldContext newContext (window: RenderWindow) =
    let diff = newContext - oldContext
    let windowPosition = window.Position
    window.Position <- windowPosition + Vector2i(diff.X, diff.Y)
    
let private performMouseOperations context =
    let window = context.RenderWindow
    let isLeftButtonClicked = SFML.Window.Mouse.IsButtonPressed (SFML.Window.Mouse.Button.Left)
    match (isLeftButtonClicked, context.MouseDragging) with
        | (true, MouseDragging mouseDraggingContext) ->
            let newMouseContext = getMouseContext()  
            moveWindow mouseDraggingContext newMouseContext window
            MouseDragging newMouseContext
        | (true, MouseNotDragging) -> getMouseContext() |> MouseDragging
        | (false, _) -> MouseNotDragging

let createSfmlRenderer =
     let window = new RenderWindow(SFML.Window.VideoMode(300u, 600u), "Block Based Game©!", SFML.Window.Styles.None)
     window.Clear()
     SFMLRenderer { RenderWindow = window; MouseDragging = MouseNotDragging } 
    
let render renderer grid =
    let getContext (SFMLRenderer window) = window
    let context = getContext renderer
    let window = context.RenderWindow
    
    window.DispatchEvents()
    
    let mouseDragging = performMouseOperations context
        
    window.Clear(Color(250uy, 250uy, 250uy))
    renderGrid window grid
    window.Display()
    SFMLRenderer {RenderWindow = window; MouseDragging = mouseDragging} 