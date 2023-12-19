
module VirtualMachine
open System.Collections.Generic
open Instructions

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
    }
    with static member Empty = {
            Stack = []
            ProgramCounter = 0
            CallStack = []
            FunctionPointer = 0s
            Functions = []
        }

let RunProgram (bytecode:byte seq) (state:State) = 
    let AssertStackRequirement state n cont = 
        if n <= List.length state.Stack 
        then cont()
        else Error "Stack underflow"   

    let ReadImmediate (bytecode: byte seq) start len converter = 
        let byteChunk = 
            bytecode
            |> Seq.skip start 
            |> Seq.take len 
            |> Seq.toArray
        if System.BitConverter.IsLittleEndian then 
            Array.rev byteChunk
        else byteChunk
        |> converter

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

    let ExtractCodeSections bytecode = 
        let count = int <| Seq.item 0 bytecode 
        let rec ReadSectionSIzes bytecode idx acc ptr=
            if idx >= count then List.rev acc 
            else 
                let sectionInput = Seq.item (1 + (idx * 4)) bytecode 
                let sectionOutput = Seq.item (2 + (idx * 4)) bytecode 
                let sectionSize = ReadImmediate bytecode (3 + (idx * 4)) 2 System.BitConverter.ToInt16
                ReadSectionSIzes bytecode (idx + 1) ({ 
                    Index = int16 idx
                    Input = sectionInput
                    Output = sectionInput
                    StartIndex = ptr
                }::acc) (ptr + int sectionSize)  
        ReadSectionSIzes bytecode 0 [] 0

    let rec Loop (machineCode:byte seq) state = 
        if state.ProgramCounter >= Seq.length machineCode 
        then Error "Bytecode has no terminating opcode" 
        else 
            let instruction : Instruction = LanguagePrimitives.EnumOfValue (int <| (Seq.item state.ProgramCounter machineCode)) 
            printfn "%A; %A; %A" machineCode state instruction
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

            | _ -> Error "Undefined opcode"

    let functions = ExtractCodeSections bytecode
    Loop (Seq.skip (1 + 4 * List.length functions) bytecode) { 
        state with  Functions = functions
    }