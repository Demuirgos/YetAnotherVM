
let bytecode : byte list = [
    02uy; 
    00uy; 00uy; 00uy; 22uy; 
    01uy; 01uy; 00uy; 07uy; 
    
    00uy; 00uy; 00uy; 00uy; 01uy; 11uy; 00uy; 05uy; 00uy; 00uy; 00uy; 00uy; 07uy; 00uy; 00uy; 00uy; 00uy; 23uy; 12uy; 00uy; 01uy; 08uy 
    01uy; 00uy; 00uy; 00uy; 00uy; 07uy; 13uy;
    ]

let result = VirtualMachine.RunProgram bytecode VirtualMachine.State.Empty

printf "%A" result