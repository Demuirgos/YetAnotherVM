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

let sumUpTo n = 
    [
        01uy; 00uy; 00uy; 00uy; 63uy;
        00uy; 00uy; 00uy; 00uy; n; 
        14uy; 00uy; 00uy; 
        00uy; 00uy; 00uy; 00uy; 0uy; 
        14uy; 00uy; 04uy;
        15uy; 00uy; 00uy;
        00uy; 00uy; 00uy; 00uy; 00uy;
        26uy; 
        11uy; 00uy; 04uy;
        15uy; 00uy; 04uy;
        08uy;
        00uy; 00uy; 00uy; 00uy; 01uy;
        15uy; 00uy; 00uy;
        16uy; 00uy; 00uy;
        17uy; 00uy; 02uy;
        17uy; 00uy; 01uy;
        05uy;
        14uy; 00uy; 00uy;
        15uy; 00uy; 04uy;
        02uy;
        14uy; 00uy; 04uy;
        10uy; 255uy; 209uy;
    ] 

let bytecodeB = 
    Build {
        Push 23
        Push 03 
        Mul
    }

printfn "%A" (BytecodeToMnemonic (sumUpTo 23uy))

let bytecode = 
    match run (fromStr inputStr) ParseProgram with
    | Success (result, _) -> printfn "%A" result; GenerateHeader (EmitBytecode result (Dictionary<_,_>(), Dictionary<_,_>()))
    | _ -> []
    |> List.ofSeq
    |> fun b -> printfn "%A" b; printfn "%s" (BytecodeToMnemonic b); b
let result = VirtualMachine.RunProgram (State.Empty bytecode)
printfn "%A" result