open Utils
open Language.Parser
open Instructions
open VirtualMachine
open parsec.Parsec
open Language.Compiler
open System.Collections.Generic

[<EntryPoint>]
let main args = 
    match args with
    | [|path|] -> 
        let inputStr = System.IO.File.ReadAllText(path)
        let bytecode = 
            match run (fromStr inputStr) ParseProgram with
            | Success (result, _) -> GenerateHeader (EmitBytecode result EmissionState.Empty)
            | _ -> []
            |> List.ofSeq
        let result = VirtualMachine.RunProgram (State.Empty bytecode)
        printfn "%A" result; 0
    | _ -> 1
