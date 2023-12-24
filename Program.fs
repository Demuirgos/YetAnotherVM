open Utils
open Language.Parser
open Instructions
open VirtualMachine
open parsec.Parsec
open Language.Compiler
open System.Collections.Generic
let inputStr = 
    """
        fun isPrime(number) {
            if((number % 2) = 0) {
                return number = 2;
            } else {
                var index = 3;
                while((index * index) < number) {
						         if((number % index) = 0) {
                        return false;
                    } 
                    index <- index + 2;
                }
                return true;
            }
        }
        return isPrime(23);
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

let bytecode = 
    match run (fromStr inputStr) ParseProgram with
    | Success (result, _) -> printfn "%A" result; GenerateHeader (EmitBytecode result (Dictionary<_,_>(), Dictionary<_,_>()))
    | _ -> []
    |> List.ofSeq
    |> fun b -> printfn "%A" b; printfn "%s" (BytecodeToMnemonic b); b
let result = VirtualMachine.RunProgram (State.Empty bytecode)
printfn "%A" result