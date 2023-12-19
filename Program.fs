
let bytecode : byte list = [00uy; 00uy; 00uy; 00uy; 01uy; 11uy; 00uy; 05uy; 00uy; 00uy; 00uy; 00uy; 07uy; 00uy; 00uy; 00uy; 00uy; 23uy; 08uy ]

let result = VirtualMachine.RunProgram bytecode VirtualMachine.State.Empty

printf "%A" result