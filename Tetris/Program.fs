open System.Diagnostics

open Tetris
open Tetris.GameModel
open Tetris.Renderer
open Tetris.Input

let processStep (stopWatch: Stopwatch) gameModel =
    if stopWatch.Elapsed.TotalSeconds > 0.5 then
        stopWatch.Reset()
        stopWatch.Start()
        stepWorld gameModel
    else
        gameModel 
       
let rec gameLoop renderer stopWatch gameModel =
    let input = Input.getProgramInput()
   
    let processStepWithStopwatch = processStep stopWatch
    let renderableGrid = renderableGrid gameModel 
    let newRendererState = render renderer renderableGrid
    let processGameLoopStep gameModel = gameModel |> processStepWithStopwatch |> (gameLoop newRendererState stopWatch)
   
    let handleProgramInput gameModel programInput =
        match programInput with
            | GameModelInput gameModeInput ->
                let processInput gameModel = reactToInput gameModel gameModeInput
                gameModel |> processInput |> processGameLoopStep
            | Quit -> gameModel 
     
    match input with
        | Some programInput-> programInput |> (handleProgramInput gameModel)
        | None -> gameModel |> processGameLoopStep

[<EntryPoint>]
let main argv =
    let renderer = createSfmlRenderer
    let gameModel = createInitialGameModel
    let stopWatch = new Stopwatch()
    stopWatch.Start()
    
    gameLoop renderer stopWatch gameModel |> ignore
    0
