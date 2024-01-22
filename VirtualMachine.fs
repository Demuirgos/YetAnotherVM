
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
        Stack : int list
        ProgramCounter : int
        CallStack : (int16 * int * int) list
        Functions : FunctionSection list
        FunctionPointer : int16
        Memory : byte[]
        Bytecode : byte list
        Error : string 
    }
    with static member Empty bytecode = {
            Stack = []
            ProgramCounter = 0
            CallStack = []
            FunctionPointer = 0s
            Functions = []
            Memory = Array.create (1024 * 1024 * 8) 0uy
            Bytecode = bytecode
            Error = System.String.Empty
        }

let RunProgram (state:State) = 
    let AssertStackRequirement state n = 
        if n <= List.length state.Stack 
        then Ok()
        else 
            let instruction : Instruction = LanguagePrimitives.EnumOfValue (int <| (Seq.item (state.ProgramCounter + 1 + int(state.Bytecode[0]) * 4) state.Bytecode)) 
            Error (sprintf "Stack underflow %d, opcode: %A" state.ProgramCounter instruction)

    let ApplyBinary state op =
        let a::b::tail = state.Stack
        let newStack = (op a b)::tail
        {
            state with Stack = newStack 
                       ProgramCounter = state.ProgramCounter + 1
        }
    
    let ApplyUnary state op =
        let a::tail = state.Stack
        let newStack = (op a)::tail
        {
            state with Stack = newStack 
                       ProgramCounter = state.ProgramCounter + 1
        }

    let JumpToPointer state conditional offset = 
        let condition = if conditional then state.Stack.Head <> 0 else true 
        let destination =  state.ProgramCounter + (if condition then 2 + offset else 2)
        {
            state with  Stack = if conditional then state.Stack.Tail else state.Stack
                        ProgramCounter = destination + 1
        }

    let rec Loop state = 
        if state.ProgramCounter >= Seq.length state.Bytecode 
        then Error "Bytecode has no terminating opcode" 
        else 
            let instruction : Instruction = LanguagePrimitives.EnumOfValue (int <| (Seq.item state.ProgramCounter state.Bytecode)) 
            match instruction with 
            | Instruction.PUSH -> 
                let argument = ReadImmediate state.Bytecode (state.ProgramCounter + 1) 4 (System.BitConverter.ToInt32)
                Loop {
                    state with  ProgramCounter = state.ProgramCounter + 5
                                Stack = argument::state.Stack
                }
            | Instruction.POP -> 
                match AssertStackRequirement state 1 with 
                | Ok _ ->  
                    Loop {
                        state with  ProgramCounter = state.ProgramCounter + 1
                                    Stack = state.Stack.Tail
                    }
                | Error(err) -> Error(err)
            | Instruction.RETURN -> 
                let index = ReadImmediate state.Bytecode (state.ProgramCounter + 1) 2 (int << System.BitConverter.ToInt16)
                let length = ReadImmediate state.Bytecode (state.ProgramCounter + 1 + 2) 2 (int << System.BitConverter.ToInt16)
                let value = ReadImmediate state.Memory index length System.BitConverter.ToInt32
                Ok <| Some (value)
            | Instruction.ADD    -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( + ))
                | Error(err) -> Error(err)
            | Instruction.MUL    -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( * ))
                | Error(err) -> Error(err)
            | Instruction.DIV    -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( / ))
                | Error(err) -> Error(err)
            | Instruction.EXP    -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state pown)
                | Error(err) -> Error(err)
            | Instruction.SUB    -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( - ))
                | Error(err) -> Error(err)
            | Instruction.MOD    -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( % ))
                | Error(err) -> Error(err)
            | Instruction.LHS    -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( <<< ))
                | Error(err) -> Error(err)
            | Instruction.RHS    -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( >>> ))
                | Error(err) -> Error(err)
            | Instruction.AND    -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( &&& ))
                | Error(err) -> Error(err)
            | Instruction.OR     -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( ||| ))
                | Error(err) -> Error(err)
            | Instruction.XOR    -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( ^^^ ))
                | Error(err) -> Error(err)
            | Instruction.NEG -> 
                match AssertStackRequirement state 1 with 
                | Ok _ ->  
                    Loop (ApplyUnary  state ( ~~~ ))
                | Error(err) -> Error(err)
            | Instruction.NOT -> 
                match AssertStackRequirement state 1 with 
                | Ok _ ->  
                    Loop (ApplyUnary state ( fun a -> if a <> 0 then 0 else 1))
                | Error(err) -> Error(err)
            | Instruction.GT  -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( fun a b -> if a > b then 1 else 0))
                | Error(err) -> Error(err)
            | Instruction.LT  -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( fun a b -> if a < b then 1 else 0))
                | Error(err) -> Error(err)
            | Instruction.EQ  -> 
                match AssertStackRequirement state 2 with 
                | Ok _ ->  
                    Loop (ApplyBinary state ( fun a b -> if a = b then 1 else 0))
                | Error(err) -> Error(err)
            | Instruction.JUMP ->  
                let target = int <| ReadImmediate state.Bytecode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                Loop (JumpToPointer state false target) 
            | Instruction.CJUMP -> 
                match AssertStackRequirement state 1 with 
                | Ok _ ->  
                    let target = int <| ReadImmediate state.Bytecode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                    Loop (JumpToPointer state true target)
                | Error(err) -> Error(err) 
            | Instruction.STOP -> Ok <| None
            | Instruction.CALL -> 
                let targetIndex = ReadImmediate state.Bytecode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                let callFrame = (state.FunctionPointer, state.ProgramCounter, List.length state.Stack)
                let targetSection = state.Functions[int targetIndex]
                match AssertStackRequirement state (int targetSection.Input) with 
                | Ok _ ->  
                    Loop {
                        state with ProgramCounter = targetSection.StartIndex
                                   CallStack = callFrame::state.CallStack
                                   FunctionPointer = targetIndex
                    }
                | Error(err) -> Error(err) 
            | Instruction.RETF -> 
                let (functionIndex, programCounter, stackSize)::rest = state.CallStack
                let currentSection = state.Functions[int state.FunctionPointer]
                match AssertStackRequirement state (stackSize - int currentSection.Input + int currentSection.Output) with 
                | Ok _ ->  
                    Loop {
                        state with ProgramCounter = programCounter + 3
                                   CallStack = rest
                                   FunctionPointer = functionIndex
                    }
                | Error(err) -> Error(err) 
            | Instruction.STORE -> 
                let isDynamic   = int <| ReadImmediate state.Bytecode (state.ProgramCounter + 1) 1 (fun bs -> bs[0])
                let targetIndex = int <| ReadImmediate state.Bytecode (state.ProgramCounter + 2) 2 (System.BitConverter.ToInt16)
                
                let stackFrame = List.length state.CallStack
                let (offset, value, stack) =
                    let (offset, stack) = 
                        if isDynamic <> 0 then 
                            (state.Stack.Head, state.Stack.Tail)
                        else (0, state.Stack)
                    
                    let value = stack.Head
                    (offset, value, stack.Tail)
                
                let valueBytes = value |> (Int32 >> getBytes) 
                System.Array.Copy(valueBytes, 0, state.Memory, ((stackFrame * 512) + offset + targetIndex), valueBytes.Length)
                match AssertStackRequirement state (((if isDynamic <> 0 then 1 else 0)) + 1) with 
                | Ok _ ->  
                    Loop {
                        state with  ProgramCounter = state.ProgramCounter + 1 + 1 + 2 
                                    Stack = stack
                    }
                | Error(err) -> Error(err) 
            | Instruction.LOAD -> 
                let isDynamic   = int <| ReadImmediate state.Bytecode (state.ProgramCounter + 1) 1 (fun bs -> bs[0])
                let targetIndex = int <| ReadImmediate state.Bytecode (state.ProgramCounter + 2) 2 (System.BitConverter.ToInt16)
                let stackFrame = List.length state.CallStack

                let (offset, stack) =
                    let (offset, stack) = 
                        if isDynamic <> 0 then 
                            (state.Stack.Head, state.Stack.Tail)
                        else (0, state.Stack)
                    
                    let value = ReadImmediate state.Memory (targetIndex + offset + (stackFrame * 512)) 4 System.BitConverter.ToInt32
                    
                    (offset, value::stack)
                
                
                Loop {
                    state with  ProgramCounter = state.ProgramCounter + 1 + 1 + 2 
                                Stack = stack
                }
            | Instruction.DUP -> 
                let targetIndex = int <| ReadImmediate state.Bytecode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                match AssertStackRequirement state targetIndex with 
                | Ok _ ->  
                    Loop {
                        state with  ProgramCounter = state.ProgramCounter + 3
                                    Stack = state.Stack[targetIndex]::state.Stack
                    }
                | Error(err) -> Error(err) 
            | Instruction.SWAP -> 
                let targetIndex = int <| ReadImmediate state.Bytecode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                match AssertStackRequirement state targetIndex with 
                | Ok _ ->  
                    let newStack =
                        state.Stack 
                        |> List.mapi (fun i v -> if i = 0 then state.Stack[targetIndex] else if i = targetIndex then state.Stack.Head else v) 

                    Loop {
                            state with  ProgramCounter = state.ProgramCounter + 3
                                        Stack = newStack
                        }
                | Error(err) -> Error(err) 
            | _ -> Error "Undefined opcode"

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