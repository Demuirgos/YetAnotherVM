module Instructions

type Int = 
    | Int32 of int32
    | Int16 of int16

type Instruction = 
    | MOV    = 01
    
    | ADD    = 04
    | MUL    = 05
    | DIV    = 06
    | SUB    = 07
    | EXP    = 08

    | MOD    = 09
    
    | RETURN = 10
    | STOP   = 11
    
    | JUMP   = 12
    | CJUMP  = 13
    
    | CALL   = 14
    | RETF   = 15
    
    | STORE  = 16
    | LOAD   = 17
    
    | NEG    = 19
    | NOT    = 20
    
    | AND    = 21
    | OR     = 22
    | XOR    = 23
    | LHS    = 24
    | RHS    = 25
    
    | GT     = 26
    | LT     = 27
    | EQ     = 28

    | DUP = 31
    | SWAP = 32

type Metadata = {
    ImmediateArgument : byte
} with static member from iArg  = { ImmediateArgument = iArg }



let GetMetadata opcode = 
    match opcode with 
    | Instruction.MOV -> Metadata.from 2uy
    | Instruction.DUP -> Metadata.from 2uy
    | Instruction.SWAP -> Metadata.from 2uy
    | Instruction.ADD
    | Instruction.MUL
    | Instruction.DIV
    | Instruction.SUB
    | Instruction.EXP
    | Instruction.GT
    | Instruction.LT
    | Instruction.AND
    | Instruction.OR
    | Instruction.XOR
    | Instruction.EQ
    | Instruction.LHS
    | Instruction.RHS    -> Metadata.from 3uy
    | Instruction.MOD    -> Metadata.from 2uy
    | Instruction.NOT    -> Metadata.from 1uy
    | Instruction.NEG    -> Metadata.from 1uy
    | Instruction.RETURN -> Metadata.from 2uy
    | Instruction.STOP   -> Metadata.from 0uy
    | Instruction.JUMP   -> Metadata.from 1uy
    | Instruction.CJUMP  -> Metadata.from 1uy
    | Instruction.CALL   -> Metadata.from 1uy
    | Instruction.RETF   -> Metadata.from 0uy
    | Instruction.STORE  -> Metadata.from 2uy
    | Instruction.LOAD   -> Metadata.from 2uy


let Op opcode (arg: int) = 
    match (GetMetadata opcode).ImmediateArgument with 
    | immediate when immediate > 0uy -> [
        byte opcode; 
        yield!  System.BitConverter.GetBytes(arg)[2..]
                |>  if System.BitConverter.IsLittleEndian then 
                        Array.rev
                    else id
        ]
    | _  -> [byte opcode;]

type BuilderState = {
    Bytecode : byte list
    Deferred : byte list list
}
let getBytes(argument:Int) =
    match argument with 
    | Int16 arg -> System.BitConverter.GetBytes(arg)
    | Int32 arg -> System.BitConverter.GetBytes(arg)
    |>  if System.BitConverter.IsLittleEndian then 
            Array.rev
        else id

type BytecodeBuilder() =

    member _.Return(value) = value
    member _.Zero() = { Bytecode = [];  Deferred = [] }
    member _.Yield _ = { Bytecode = [];  Deferred = [] }


    [<CustomOperation("Signature")>]
    member _.Signature (source: BuilderState, (inputCount:byte, outputCount:byte)) = { source with Bytecode = [inputCount; outputCount]@source.Bytecode }
    
    [<CustomOperation("Mov")>]
    member _.Mov(source: BuilderState, register1:int, register2: int) = { source with Bytecode = source.Bytecode@[01uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2)]}
    [<CustomOperation("Dup")>]
    member _.Dup(source: BuilderState, register1:int, register2: int) = { source with Bytecode = source.Bytecode@[31uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2)]}
    [<CustomOperation("Swap")>]
    member _.Swap(source: BuilderState, register1:int, register2: int) = { source with Bytecode = source.Bytecode@[32uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2)]}
    [<CustomOperation("Add")>]
    member _.Add(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[04uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Mul")>]
    member _.Mul(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[05uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Div")>]
    member _.Div(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[06uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Sub")>]
    member _.Sub(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[07uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Exp")>]
    member _.Exp(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[08uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Gt")>]
    member _.Gt(source: BuilderState, register1:int, register2: int) = { source with Bytecode = source.Bytecode@[26uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2)]}
    [<CustomOperation("Lt")>]
    member _.Lt(source: BuilderState, register1:int, register2: int) = { source with Bytecode = source.Bytecode@[27uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2)]}
    [<CustomOperation("And")>]
    member _.And(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[21uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Or")>]
    member _.Or(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[22uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Xor")>]
    member _.Xor(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[23uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Eq")>]
    member _.Eq(source: BuilderState, register1:int, register2: int) = { source with Bytecode = source.Bytecode@[28uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2)]}
    [<CustomOperation("Lhs")>]
    member _.Lhs(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[24uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Rhs")>]
    member _.Rhs(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[25uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Mod")>]
    member _.Mod(source: BuilderState, register1:int, register2: int) = { source with Bytecode = source.Bytecode@[09uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2)]}
    [<CustomOperation("Not")>]
    member _.Not(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[20uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Neg")>]
    member _.Neg(source: BuilderState, register1:int, register2: int) = { source with Bytecode = source.Bytecode@[19uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2)]}
    [<CustomOperation("Return")>]
    member _.Return(source: BuilderState, offset:int, len: int) = { source with Bytecode = source.Bytecode@[10uy; yield! getBytes(Int32 offset); yield! getBytes(Int32 len)]}
    [<CustomOperation("Stop")>]
    member _.Stop(source: BuilderState, register1:int, register2: int, register3:int) = { source with Bytecode = source.Bytecode@[11uy; yield! getBytes(Int32 register1); yield! getBytes(Int32 register2); yield! getBytes(Int32 register3)]}
    [<CustomOperation("Jump")>]
    member _.Jump(source: BuilderState, offset:int) = { source with Bytecode = source.Bytecode@[13uy; yield! getBytes(Int32 offset)]}
    [<CustomOperation("Cjump")>]
    member _.Cjump(source: BuilderState, offset:int) = { source with Bytecode = source.Bytecode@[13uy; yield! getBytes(Int32 offset)]}
    [<CustomOperation("Call")>]
    member _.Call(source: BuilderState, functionId:int) = { source with Bytecode = source.Bytecode@[14uy; yield! getBytes(Int32 functionId)]}
    [<CustomOperation("Retf")>]
    member _.Retf(source: BuilderState) = { source with Bytecode = source.Bytecode@[15uy]}
    [<CustomOperation("Store")>]
    member _.Store(source: BuilderState, targetRegister:int, registerIndex: int) = { source with Bytecode = source.Bytecode@[16uy; yield! getBytes(Int32 targetRegister); yield! getBytes(Int32 registerIndex)]}
    [<CustomOperation("Load")>]
    member _.Load(source: BuilderState, registerIndex:int, targetRegister: int) = { source with Bytecode = source.Bytecode@[17uy; yield! getBytes(Int32 registerIndex); yield! getBytes(Int32 targetRegister)]}


    [<CustomOperation("Inline")>]
    member _.Inline(source: BuilderState, bytecode: byte list)= { source with Bytecode = source.Bytecode@bytecode }
    [<CustomOperation("Op")>]
    member _.Op(source: BuilderState, opcode: Instruction)= { source with Bytecode = source.Bytecode@[byte opcode] }
    [<CustomOperation("Defer")>]
    member _.Defer(source: BuilderState, functionCode: byte list)= { source with Deferred = functionCode::source.Deferred }




 
 let Build = BytecodeBuilder()