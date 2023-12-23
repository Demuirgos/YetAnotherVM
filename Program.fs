open Utils
open parsec.Parsec
open Language.Parser
open Instructions
open VirtualMachine

let bytecode = 
    let result = 
        FunctionSection.GenerateHeader 
            (Build {
                Signature (00uy, 00uy)
                Push 23
                Push 03
                Mul
                Pop 
            })
    printfn "%A" result; result

let inputStr = 
    """
        var test = 69; 
        fun function(x,y) 
        { 
            if (true) {
                var nice = 23 * 3;
                nice <- 23; 
            }
        }
    """
let result = parsec.Parsec.Parser.run (fromStr inputStr) ParseProgram
printf "%A" result