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
            Build {
                Mov 0x00 23
                Mov 0x01 3
                Mul 0x00 0x01 0x02
                Store 0x02 0x00
                Load 0x00 0x03 
                Dup 0x01 0x04
                Swap 0x04 0x00
            }
        let bytecode = 
            match run (fromStr inputStr) ParseProgram with
            | Success (result, _) -> GenerateHeader (EmitBytecode result EmissionState.Empty)
            | _ -> []
            |> List.ofSeq
        let result = VirtualMachine.RunProgram (State.Empty bytecode)
        printfn "%A" result; 0
    | _ -> 1
