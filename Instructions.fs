module Instructions

type Instruction = 
    | PUSH   = 00 
    | POP    = 01

    | ADD    = 02
    | MUL    = 03
    | DIV    = 04
    | SUB    = 05
    | EXP    = 06
    | MOD    = 07
    
    | RETURN = 08
    | STOP   = 09
    
    | JUMP   = 10
    | CJUMP  = 11
    
    | CALL   = 12
    | RETF   = 13
    
    | STORE  = 14
    | LOAD   = 15
    | DUP    = 16
    | SWAP   = 17

    | FAIL   = 18
    
    | NEG    = 19
    | AND    = 20
    | OR     = 21
    | XOR    = 22

    | LHS    = 23
    | RHS    = 24
    
    | GT     = 25
    | LT     = 26
    | EQ     = 27

type Metadata = {
    StackArgument : byte
    ImmediateArgument : byte
    StackOutput : byte
} with static member from sArg iArg sOut = {StackArgument = sArg; ImmediateArgument = iArg; StackOutput = sOut}

let GetMetadata opcode = 
    match opcode with 
    | Instruction.PUSH -> Metadata.from 0uy 4uy 1uy
    | Instruction.POP  -> Metadata.from 1uy 0uy 0uy
    | Instruction.ADD
    | Instruction.MUL
    | Instruction.DIV
    | Instruction.SUB
    | Instruction.EXP
    | Instruction.GT
    | Instruction.LT
    | Instruction.GT
    | Instruction.AND
    | Instruction.OR
    | Instruction.XOR
    | Instruction.EQ
    | Instruction.LHS
    | Instruction.RHS
    | Instruction.MOD   -> Metadata.from 2uy 0uy 1uy
    | Instruction.NEG   -> Metadata.from 1uy 0uy 1uy
    | Instruction.RETURN -> Metadata.from 1uy 0uy 0uy
    | Instruction.STOP  -> Metadata.from 0uy 0uy 0uy
    | Instruction.JUMP  -> Metadata.from 0uy 2uy 0uy
    | Instruction.CJUMP -> Metadata.from 1uy 2uy 0uy
    | Instruction.CALL  -> Metadata.from 0uy 2uy 0uy
    | Instruction.RETF  -> Metadata.from 0uy 0uy 0uy
    | Instruction.STORE -> Metadata.from 1uy 2uy 0uy
    | Instruction.LOAD  -> Metadata.from 0uy 2uy 1uy
    | Instruction.DUP   -> Metadata.from 1uy 2uy 1uy
    | Instruction.SWAP   -> Metadata.from 1uy 2uy 1uy
    | Instruction.FAIL   -> Metadata.from 0uy 0uy 0uy
    | _ as instr -> printfn "%A" instr; failwith "invalid opcode"
