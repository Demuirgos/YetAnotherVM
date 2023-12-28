module Instructions

type Int = 
    | Int32 of int32
    | Int16 of int16

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
    | NOT    = 28
    | AND    = 20
    | OR     = 21
    | XOR    = 22

    | LHS    = 23
    | RHS    = 24
    
    | GT     = 25
    | LT     = 26
    | EQ     = 27

    | INPUT  = 30

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
    | Instruction.NOT
    | Instruction.NEG   -> Metadata.from 1uy 0uy 1uy
    | Instruction.RETURN -> Metadata.from 1uy 0uy 0uy
    | Instruction.STOP  -> Metadata.from 0uy 0uy 0uy
    | Instruction.JUMP  -> Metadata.from 0uy 2uy 0uy
    | Instruction.CJUMP -> Metadata.from 1uy 2uy 0uy
    | Instruction.CALL  -> Metadata.from 0uy 2uy 0uy
    | Instruction.RETF  -> Metadata.from 0uy 0uy 0uy
    | Instruction.STORE -> Metadata.from 1uy 4uy 0uy
    | Instruction.LOAD  -> Metadata.from 0uy 4uy 1uy
    | Instruction.DUP   -> Metadata.from 1uy 2uy 1uy
    | Instruction.SWAP  -> Metadata.from 1uy 2uy 1uy
    | Instruction.FAIL  -> Metadata.from 0uy 4uy 0uy
    | Instruction.INPUT -> Metadata.from 0uy 0uy 1uy
    | _ as instr -> printfn "%A" instr; failwith "invalid opcode"


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
    [<CustomOperation("Push")>]
    member _.Push (source: BuilderState, argument:int32) = { source with Bytecode = source.Bytecode@[00uy; yield! getBytes(Int32 argument)]}
    [<CustomOperation("Pop")>]
    member _.Pop(source: BuilderState) = { source with Bytecode = source.Bytecode@[01uy] }
    [<CustomOperation("Read")>]
    member _.Read(source: BuilderState) = { source with Bytecode = source.Bytecode@[30uy] }
    [<CustomOperation("Add")>]
    member _.Add(source: BuilderState) = { source with Bytecode = source.Bytecode@[02uy] }
    [<CustomOperation("Mul")>]
    member _.Mul(source: BuilderState) = { source with Bytecode = source.Bytecode@[03uy] }
    [<CustomOperation("Div")>]
    member _.Div(source: BuilderState) = { source with Bytecode = source.Bytecode@[04uy] }
    [<CustomOperation("Sub")>]
    member _.Sub(source: BuilderState) = { source with Bytecode = source.Bytecode@[05uy] }
    [<CustomOperation("Exp")>]
    member _.Exp(source: BuilderState) = { source with Bytecode = source.Bytecode@[06uy] }
    [<CustomOperation("Mod")>]
    member _.Mod(source: BuilderState) = { source with Bytecode = source.Bytecode@[07uy] }
    [<CustomOperation("Return")>]
    member _.Return(source: BuilderState) = { source with Bytecode = source.Bytecode@[08uy] }
    [<CustomOperation("Stop")>]
    member _.Stop(source: BuilderState) = { source with Bytecode = source.Bytecode@[09uy] }
    [<CustomOperation("Fail")>]
    member _.Fail(source: BuilderState, idx: int16, len: int16) = { source with  Bytecode = source.Bytecode@[18uy; yield! getBytes(Int16 idx); yield! getBytes(Int16 len) ]}
    [<CustomOperation("Neg")>]
    member _.Neg(source: BuilderState) = { source with Bytecode = source.Bytecode@[19uy] }
    [<CustomOperation("And")>]
    member _.And(source: BuilderState) = { source with Bytecode = source.Bytecode@[20uy] }
    [<CustomOperation("Or")>]
    member _.Or(source: BuilderState) = { source with Bytecode = source.Bytecode@[21uy] }
    
    [<CustomOperation("Not")>]
    member _.Not(source: BuilderState) = { source with Bytecode = source.Bytecode@[28uy] }

    [<CustomOperation("Xor")>]
    member _.Xor(source: BuilderState) = { source with Bytecode = source.Bytecode@[22uy] }
    [<CustomOperation("Lhs")>]
    member _.Lhs(source: BuilderState) = { source with Bytecode = source.Bytecode@[23uy] }
    [<CustomOperation("Rhs")>]
    member _.Rhs(source: BuilderState) = { source with Bytecode = source.Bytecode@[24uy] }
    [<CustomOperation("Gt")>]
    member _.Gt(source: BuilderState) = { source with Bytecode = source.Bytecode@[25uy] }
    [<CustomOperation("Lt")>]
    member _.Lt(source: BuilderState) = { source with Bytecode = source.Bytecode@[26uy] }
    [<CustomOperation("Eq")>]
    member _.Eq(source: BuilderState) = { source with Bytecode = source.Bytecode@[27uy] }

    [<CustomOperation("Dup")>]
    member _.Dup(source: BuilderState, argument: int16)= { source with Bytecode = source.Bytecode@[16uy; yield! getBytes(Int16 argument)]}
    [<CustomOperation("Swap")>]
    member _.Swap(source: BuilderState, argument: int16)= { source with Bytecode = source.Bytecode@[17uy; yield! getBytes(Int16 argument)]}
    [<CustomOperation("Store")>]
    member _.Store(source: BuilderState, address: int16, count: int16)= { source with Bytecode = source.Bytecode@[14uy; yield! getBytes(Int16 address); yield! getBytes(Int16 count)]}
    [<CustomOperation("Load")>]
    member _.Load(source: BuilderState, address:int16, count:int16)= { source with Bytecode = source.Bytecode@[15uy; yield! getBytes(Int16 address); yield! getBytes(Int16 count)]}
    [<CustomOperation("DStore")>]
    member _.DStore(source: BuilderState, argument: int16)= { source with Bytecode = source.Bytecode@[29uy]}
    [<CustomOperation("DLoad")>]
    member _.DLoad(source: BuilderState, argument: int16)= { source with Bytecode = source.Bytecode@[30uy]}
    [<CustomOperation("Jump")>]
    member _.Jump(source: BuilderState, argument: int16)= { source with Bytecode = source.Bytecode@[10uy; yield! getBytes(Int16 argument)]}
    [<CustomOperation("Cjump")>]
    member _.Cjump(source: BuilderState, argument: int16)= { source with Bytecode = source.Bytecode@[11uy; yield! getBytes(Int16 argument)]}
    [<CustomOperation("Call")>]
    member _.Call(source: BuilderState, argument: int16)= { source with Bytecode = source.Bytecode@[12uy; yield! getBytes(Int16 argument)]}
    [<CustomOperation("Retf")>]
    member _.Retf(source: BuilderState)= { source with Bytecode = source.Bytecode@[13uy]}

    [<CustomOperation("Inline")>]
    member _.Inline(source: BuilderState, bytecode: byte list)= { source with Bytecode = source.Bytecode@bytecode }
    [<CustomOperation("Op")>]
    member _.Op(source: BuilderState, opcode: Instruction)= { source with Bytecode = source.Bytecode@[byte opcode] }
    [<CustomOperation("Defer")>]
    member _.Defer(source: BuilderState, functionCode: byte list)= { source with Deferred = functionCode::source.Deferred }




 
 let Build = BytecodeBuilder()