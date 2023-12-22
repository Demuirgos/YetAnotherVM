open Utils
open parsec.Parsec
open Language

let bytecode : byte list = [
    02uy; 
    00uy; 00uy; 00uy; 28uy; 
    01uy; 01uy; 00uy; 15uy; 
    
    00uy; 00uy; 00uy; 00uy; 01uy; 16uy; 00uy; 01uy; 14uy; 00uy; 00uy; 11uy; 00uy; 05uy; 00uy; 00uy; 00uy; 00uy; 07uy; 00uy; 00uy; 00uy; 00uy; 23uy; 12uy; 00uy; 01uy; 08uy 
    01uy; 15uy; 00uy; 00uy; 00uy; 00uy; 00uy; 00uy; 07uy; 17uy; 00uy; 01uy; 01uy; 18uy; 13uy;
    ]

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

printfn "%A" (BytecodeToMnemonic (bytecode))

let result = VirtualMachine.RunProgram (sumUpTo 23uy) VirtualMachine.State.Empty

printf "%A" result
