open Utils
open Language.Parser
open Instructions
open VirtualMachine
open parsec.Parsec
open Language.Compiler
open System.Collections.Generic
let inputStr = 
    """
        fun add(a,b) {
            a <- a + b;
            return a;
        }
        var condition = true;
        var result = 0;
        while(condition) {
            result <- add(result,1);
            if(result = 23) {
                condition <- false;
            }    
        }
        return result;
    """
let bytecode = 
    match run (fromStr inputStr) ParseProgram with
    | Success (result, _) -> printfn "%A" result; GenerateHeader (EmitBytecode result (Dictionary<_,_>(), Dictionary<_,_>()))
    | _ -> []
    |> List.ofSeq
    |> fun b -> printfn "%A" b; printfn "%s" (BytecodeToMnemonic b); b
let result = VirtualMachine.RunProgram (State.Empty bytecode)
printfn "%A" result