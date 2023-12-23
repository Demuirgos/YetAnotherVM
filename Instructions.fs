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

type BytecodeBuilder() =
    let getBytes(argument:Int) =
        match argument with 
        | Int16 arg -> System.BitConverter.GetBytes(arg)
        | Int32 arg -> System.BitConverter.GetBytes(arg)
        |>  if System.BitConverter.IsLittleEndian then 
                Array.rev
            else id

    member x.Return(value) = value
    member x.Zero() = []
    member _.Yield _ = []


    [<CustomOperation("Signature")>]
    member x.Signature (source: byte list, (inputCount:byte, outputCount:byte)) = [inputCount; outputCount]
    
    [<CustomOperation("Push")>]
    member x.Push (source: byte list, argument:int32) = source@[00uy; yield! getBytes(Int32 argument)]
    
    [<CustomOperation("Pop")>]
    member _.Pop(source: byte list) = source@[01uy]
    [<CustomOperation("Add")>]
    member _.Add(source: byte list) = source@[02uy]
    [<CustomOperation("Mul")>]
    member _.Mul(source: byte list) = source@[03uy]
    [<CustomOperation("Div")>]
    member _.Div(source: byte list) = source@[04uy]
    [<CustomOperation("Sub")>]
    member _.Sub(source: byte list) = source@[05uy]
    [<CustomOperation("Exp")>]
    member _.Exp(source: byte list) = source@[06uy]
    [<CustomOperation("Mod")>]
    member _.Mod(source: byte list) = source@[07uy]
    [<CustomOperation("Return")>]
    member _.Return(source: byte list) = source@[08uy]
    [<CustomOperation("Stop")>]
    member _.Stop(source: byte list) = source@[09uy]
    [<CustomOperation("Fail")>]
    member _.Fail(source: byte list) = source@[18uy]
    [<CustomOperation("Neg")>]
    member _.Neg(source: byte list) = source@[19uy]
    [<CustomOperation("And")>]
    member _.And(source: byte list) = source@[20uy]
    [<CustomOperation("Or")>]
    member _.Or(source: byte list) = source@[21uy]
    [<CustomOperation("Xor")>]
    member _.Xor(source: byte list) = source@[22uy]
    [<CustomOperation("Lhs")>]
    member _.Lhs(source: byte list) = source@[23uy]
    [<CustomOperation("Rhs")>]
    member _.Rhs(source: byte list) = source@[24uy]
    [<CustomOperation("Gt")>]
    member _.Gt(source: byte list) = source@[25uy]
    [<CustomOperation("Lt")>]
    member _.Lt(source: byte list) = source@[26uy]
    [<CustomOperation("Eq")>]
    member _.Eq(source: byte list) = source@[27uy]

    [<CustomOperation("Dup")>]
    member _.Dup(source: byte list, argument: int16)= source@[16uy; yield! getBytes(Int16 argument)]
    [<CustomOperation("Swap")>]
    member _.Swap(source: byte list, argument: int16)= source@[17uy; yield! getBytes(Int16 argument)]
    [<CustomOperation("Store")>]
    member _.Store(source: byte list, argument: int16)= source@[14uy; yield! getBytes(Int16 argument)]
    [<CustomOperation("Load")>]
    member _.Load(source: byte list, argument: int16)= source@[15uy; yield! getBytes(Int16 argument)]
    [<CustomOperation("Jump")>]
    member _.Jump(source: byte list, argument: int16)= source@[10uy; yield! getBytes(Int16 argument)]
    [<CustomOperation("Cjump")>]
    member _.Cjump(source: byte list, argument: int16)= source@[11uy; yield! getBytes(Int16 argument)]
    [<CustomOperation("Call")>]
    member _.Call(source: byte list, argument: int16)= source@[12uy; yield! getBytes(Int16 argument)]
    [<CustomOperation("Retf")>]
    member _.Retf(source: byte list, argument: int16)= source@[13uy; yield! getBytes(Int16 argument)]
 
 let Build = BytecodeBuilder()