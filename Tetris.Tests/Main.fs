module Tetris.Tests.Runner
open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
