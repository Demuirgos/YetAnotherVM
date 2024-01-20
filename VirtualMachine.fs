
module VirtualMachine
open System
open Instructions
open Utils

type FunctionSection = {
        Index : int16
        StartIndex : int
        Input : byte
        Output : byte
    } 

type State = {
        Registers : int[]
        Memory : byte[]
        ProgramCounter : int
        CallStack : (int16 * int) list
        Functions : FunctionSection list
        FunctionPointer : int16
        Bytecode : byte list
        Error : string 
    }
    with static member Empty bytecode = {
            Registers = Array.create 8 0 // 6 regular registers and 1 remainder and 1 boolean
            ProgramCounter = 0
            CallStack = []
            FunctionPointer = 0s
            Functions = []
            Memory = Array.create (1024 * 1024 * 8) 0uy
            Bytecode = bytecode
            Error = System.String.Empty
        }

let RunProgram (state:State) = 
    let ApplyBinary state operation lPtr rPtr dPtr =
        let (a, b) = (state.Registers[lPtr], state.Registers[rPtr]) 
        let (isDiv, op) = operation
        state.Registers[dPtr] <- op a b
        if isDiv then 
            state.Registers[6] <- a % b

    let JumpToPointer state conditional offset = 
        let condition = if conditional then state.Registers[7] <> 0 else true 
        let destination =  state.ProgramCounter + (if condition then 2 + offset else 2)
        {
            state with  ProgramCounter = destination + 1
        }

    let rec Loop state = 
        if state.ProgramCounter >= Seq.length state.Bytecode 
        then Error "Bytecode has no terminating opcode" 
        else 
            let instruction : Instruction = LanguagePrimitives.EnumOfValue (int <| (Seq.item state.ProgramCounter state.Bytecode)) 
            match instruction with 
            | Instruction.MOV ->
                let value = ReadImmediate state.Bytecode (state.ProgramCounter + 1) 4 (System.BitConverter.ToInt32)
                let register = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 4) 4 (System.BitConverter.ToInt32)
                
                Array.set state.Registers register value
                Loop {
                    state with  ProgramCounter = state.ProgramCounter + 1 + 4 + 4
                                Registers = state.Registers 
                }
            | Instruction.DUP ->
                let register1 = ReadImmediate state.Bytecode (state.ProgramCounter + 1) 4 (System.BitConverter.ToInt32)
                let register2 = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 4) 4 (System.BitConverter.ToInt32)
                
                Array.set state.Registers register2 (state.Registers[register1])
                Loop {
                    state with  ProgramCounter = state.ProgramCounter + 1 + 4 + 4
                                Registers = state.Registers 
                }
            | Instruction.SWAP ->
                let register1 = ReadImmediate state.Bytecode (state.ProgramCounter + 1) 4 (System.BitConverter.ToInt32)
                let register2 = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 4) 4 (System.BitConverter.ToInt32)
                
                let reg1Value = state.Registers[register1]
                let reg2Value = state.Registers[register2]

                state.Registers[register2] <- reg1Value
                state.Registers[register2] <- reg2Value

                Loop {
                    state with  ProgramCounter = state.ProgramCounter + 1 + 4 + 4
                                Registers = state.Registers 
                }

            | Instruction.ADD | Instruction.MUL | Instruction.DIV | Instruction.SUB as opcode->
                let (isDiv, op) as operation= 
                    match opcode with 
                    | Instruction.SUB -> false, ( - )
                    | Instruction.ADD -> false, ( + )
                    | Instruction.MUL -> false, ( * )
                    | Instruction.DIV -> true , ( / )
                let registerArg = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 0) 4 (System.BitConverter.ToInt32)
                let registerAcc = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 4) 4 (System.BitConverter.ToInt32)
                
                ApplyBinary state operation registerArg registerAcc registerAcc
                Loop {
                    state with  ProgramCounter = state.ProgramCounter + 1 + 4 + 4
                                Registers = state.Registers 
                }
            | Instruction.GT | Instruction.LT | Instruction.EQ as opcode ->
                let (isDiv, op) as operation= 
                    match opcode with 
                    | Instruction.LT -> false, fun a b -> if (a < b) then 1 else 0 
                    | Instruction.GT -> false, fun a b -> if (a > b) then 1 else 0
                    | Instruction.EQ -> false, fun a b -> if (a = b) then 1 else 0
                let registerArg = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 0) 4 (System.BitConverter.ToInt32)
                let registerAcc = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 4) 4 (System.BitConverter.ToInt32)
                
                ApplyBinary state operation registerArg registerAcc 7
                Loop {
                    state with  ProgramCounter = state.ProgramCounter + 1 + 4 + 4
                                Registers = state.Registers 
                }
            | Instruction.LHS | Instruction.RHS as opcode ->
                let (isDiv, op) as operation= 
                    match opcode with 
                    | Instruction.RHS -> false, (>>>) 
                    | Instruction.LHS -> false, (<<<)

                let registerArg = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 0) 4 (System.BitConverter.ToInt32)
                let registerAcc = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 4) 4 (System.BitConverter.ToInt32)
                
                ApplyBinary state operation registerArg registerAcc 7
                Loop {
                    state with  ProgramCounter = state.ProgramCounter + 1 + 4 + 4
                                Registers = state.Registers 
                }
            | Instruction.NEG as opcode ->
                let registerArg = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 0) 4 (System.BitConverter.ToInt32)
                let registerAcc = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 4) 4 (System.BitConverter.ToInt32)
                
                state.Registers[registerAcc] <- ~state.Registers[registerArg]
                Loop {
                    state with  ProgramCounter = state.ProgramCounter + 1 + 4 + 4
                                Registers = state.Registers 
                }
            
            | Instruction.CJUMP | Instruction.JUMP as opcode ->
                let jumpdest = ReadImmediate state.Bytecode (state.ProgramCounter + 1) 4 (System.BitConverter.ToInt32)
                Loop <| JumpToPointer state (opcode = Instruction.CJUMP) jumpdest
            | Instruction.STOP -> Ok <| None
            | Instruction.NOT -> 
                state.Registers[7] <- if state.Registers[7] > 0 then 0 else 1
                Loop {
                    state with  ProgramCounter = state.ProgramCounter + 1 
                                Registers = state.Registers 
                }
            | Instruction.CALL -> 
                let targetIndex = ReadImmediate state.Bytecode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                let callFrame = (state.FunctionPointer, state.ProgramCounter)
                let targetSection = state.Functions[int targetIndex]
                Loop {
                    state with  ProgramCounter = targetSection.StartIndex
                                CallStack = callFrame::state.CallStack
                                FunctionPointer = targetIndex
                }
            | Instruction.RETF -> 
                let (functionIndex, programCounter)::rest = state.CallStack
                let currentSection = state.Functions[int state.FunctionPointer]
                Loop {
                    state with  ProgramCounter = programCounter + 3
                                CallStack = rest
                                FunctionPointer = functionIndex
                }
            | Instruction.RETURN -> 
                let registerPtr = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 0) 4 (System.BitConverter.ToInt32)
                let registerLen = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 4) 4 (System.BitConverter.ToInt32)
                let memorySlice  = 
                    state.Memory 
                    |> Array.skip state.Registers[registerPtr]    
                    |> Array.take state.Registers[registerLen]    
                Ok (Some memorySlice)
            | Instruction.STORE -> 
                let registerTarget = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 0) 4 (System.BitConverter.ToInt32)
                let registerIndex = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 4) 4 (System.BitConverter.ToInt32)
                state.Memory[state.Registers[registerIndex]] <- state.Registers[registerTarget]
                Loop {
                    state with  ProgramCounter = programCounter + 1 + 4 + 4
                                Memory = state.Memory
                } 
            | Instruction.STORE -> 
                let registerIndex = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 0) 4 (System.BitConverter.ToInt32)
                let registerTarget = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 4) 4 (System.BitConverter.ToInt32)
                state.Registers[registerTarget] <- state.Memory[state.Registers[registerIndex]] 
                Loop {
                    state with  ProgramCounter = programCounter + 1 + 4 + 4
                                Memory = state.Memory
                } 
    let functions = 
        ExtractCodeSections state.Bytecode (fun index inputCount outputCount size ptr code -> { 
            Index = int16 index
            Input = inputCount
            Output = outputCount
            StartIndex = ptr
        }) 

    Loop { 
        state with  Functions = functions
                    Bytecode = Seq.skip (1 + 4 * List.length functions) state.Bytecode |> Seq.toList
    }