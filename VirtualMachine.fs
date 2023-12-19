
module VirtualMachine
open System.Collections.Generic
open Instructions
open Outils

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
    }
    with static member Empty = {
            Stack = []
            ProgramCounter = 0
            CallStack = []
            FunctionPointer = 0s
            Functions = []
            Memory = Array.create 2048 0uy
        }

let RunProgram (bytecode:byte seq) (state:State) = 
    let AssertStackRequirement state n cont = 
        if n <= List.length state.Stack 
        then cont()
        else Error "Stack underflow"   

    let ApplyBinary state op =
        let a::b::tail = state.Stack
        let newStack = (op a b)::tail
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

    let rec Loop (machineCode:byte seq) state = 
        if state.ProgramCounter >= Seq.length machineCode 
        then Error "Bytecode has no terminating opcode" 
        else 
            let instruction : Instruction = LanguagePrimitives.EnumOfValue (int <| (Seq.item state.ProgramCounter machineCode)) 
            match instruction with 
            | Instruction.PUSH -> 
                let argument = ReadImmediate machineCode (state.ProgramCounter + 1) 4 (System.BitConverter.ToInt32)
                Loop machineCode {
                    state with  ProgramCounter = state.ProgramCounter + 5
                                Stack = argument::state.Stack
                }
            | Instruction.POP -> 
                AssertStackRequirement state 1 (fun () -> Loop machineCode {
                    state with  ProgramCounter = state.ProgramCounter + 1
                                Stack = state.Stack.Tail
                })
            | Instruction.RETURN -> AssertStackRequirement state 1 (fun () -> Ok <| Some (state.Stack.Head))

            | Instruction.ADD -> AssertStackRequirement state 2 (fun () -> Loop machineCode (ApplyBinary state ( + )))
            | Instruction.MUL -> AssertStackRequirement state 2 (fun () -> Loop machineCode (ApplyBinary state ( * )))
            | Instruction.DIV -> AssertStackRequirement state 2 (fun () -> Loop machineCode (ApplyBinary state ( / )))
            | Instruction.EXP -> AssertStackRequirement state 2 (fun () -> Loop machineCode (ApplyBinary state pown))
            | Instruction.SUB -> AssertStackRequirement state 2 (fun () -> Loop machineCode (ApplyBinary state ( - )))
            | Instruction.MOD -> AssertStackRequirement state 2 (fun () -> Loop machineCode (ApplyBinary state ( % )))

            | Instruction.JUMP ->  
                let target = int <| ReadImmediate machineCode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                Loop machineCode (JumpToPointer state true target) 
            | Instruction.CJUMP ->  
                AssertStackRequirement state 1 (fun () -> 
                    let condition::_ = state.Stack
                    let target = int <| ReadImmediate machineCode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                    Loop machineCode (JumpToPointer state (condition <> 0) target))
            | Instruction.STOP -> Ok <| None
            | Instruction.CALL -> 
                let targetIndex = ReadImmediate machineCode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                let callFrame = (state.FunctionPointer, state.ProgramCounter, List.length state.Stack)
                let targetSection = state.Functions[int targetIndex]
                AssertStackRequirement state (int targetSection.Input) (fun () -> 
                    Loop machineCode {
                        state with ProgramCounter = targetSection.StartIndex
                                   CallStack = callFrame::state.CallStack
                                   FunctionPointer = targetIndex
                    }
                )
            | Instruction.RETF -> 
                let (functionIndex, programCounter, stackSize)::rest = state.CallStack
                let currentSection = state.Functions[int functionIndex]
                AssertStackRequirement state (stackSize + int currentSection.Output) (fun () -> 
                    Loop machineCode {
                        state with ProgramCounter = programCounter + 3
                                   CallStack = rest
                                   FunctionPointer = functionIndex
                    }
                )
            | Instruction.STORE -> 
                let targetIndex = ReadImmediate machineCode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                let value::rest = state.Stack
                System.Array.Copy(System.BitConverter.GetBytes(value), 0, state.Memory, int targetIndex, 4)
                AssertStackRequirement state 1 (fun () -> 
                    Loop machineCode {
                        state with  ProgramCounter = state.ProgramCounter + 3
                                    Stack = rest
                    }
                )
            | Instruction.LOAD -> 
                let targetIndex = int <| ReadImmediate machineCode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                let value = System.BitConverter.ToInt32(state.Memory[targetIndex..(targetIndex + 4)])
                Loop machineCode {
                    state with  ProgramCounter = state.ProgramCounter + 3
                                Stack = value::state.Stack
                }
            | Instruction.DUP -> 
                let targetIndex = int <| ReadImmediate machineCode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                AssertStackRequirement state targetIndex (fun () -> 
                    Loop machineCode {
                        state with  ProgramCounter = state.ProgramCounter + 3
                                    Stack = state.Stack[state.Stack.Length - targetIndex]::state.Stack
                    }
                )
            | Instruction.SWAP -> 
                let targetIndex = int <| ReadImmediate machineCode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                AssertStackRequirement state targetIndex (fun () -> 
                    let newStack =
                        state.Stack 
                        |> List.mapi (fun i v -> if i = 0 then state.Stack[targetIndex] else state.Stack.Head) 

                    Loop machineCode {
                            state with  ProgramCounter = state.ProgramCounter + 3
                                        Stack = List.rev newStack
                        }
                )
            | Instruction.FAIL -> Error "exception throw"
            | _ -> Error "Undefined opcode"

    let functions = 
        ExtractCodeSections bytecode (fun index inputCount outputCount size ptr code -> { 
            Index = int16 index
            Input = inputCount
            Output = outputCount
            StartIndex = ptr
        }) 

    Loop (Seq.skip (1 + 4 * List.length functions) bytecode) { 
        state with  Functions = functions
    }