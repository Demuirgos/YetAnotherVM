open Outils

let bytecode : byte list = [
    02uy; 
    00uy; 00uy; 00uy; 28uy; 
    01uy; 01uy; 00uy; 14uy; 
    
    00uy; 00uy; 00uy; 00uy; 01uy; 16uy; 00uy; 01uy; 14uy; 00uy; 00uy; 11uy; 00uy; 05uy; 00uy; 00uy; 00uy; 00uy; 07uy; 00uy; 00uy; 00uy; 00uy; 23uy; 12uy; 00uy; 01uy; 08uy 
    01uy; 15uy; 00uy; 00uy; 00uy; 00uy; 00uy; 00uy; 07uy; 17uy; 00uy; 01uy; 01uy; 13uy;
    ]

printfn "%A" (BytecodeToMnemonic bytecode)

let result = VirtualMachine.RunProgram bytecode VirtualMachine.State.Empty

printf "%A" result