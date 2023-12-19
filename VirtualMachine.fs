
module VirtualMachine
open System.Collections.Generic
open Instructions

type State = {
        Stack : int list
        ProgramCounter : int
    }
    with static Empty = {
            Stack = []
            ProgramCounter = 0
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

    let rec Loop (machineCode:byte seq) state = 
        if state.ProgramCounter >= Seq.length machineCode 
        then Error "Bytecode has no terminating opcode" 
        else 
            let instruction : Instruction = LanguagePrimitives.EnumOfValue (int <| (Seq.item state.ProgramCounter machineCode)) 
            printfn "%A; %A" state instruction
            match instruction with 
            | Instruction.PUSH -> 
                let argument = ReadImmediate bytecode (state.ProgramCounter + 1) 4 (System.BitConverter.ToInt32)
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
                let target = int <| ReadImmediate bytecode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                Loop bytecode (JumpToPointer state true target) 
            | Instruction.CJUMP ->  
                AssertStackRequirement state 1 (fun () -> 
                    let condition::_ = state.Stack
                    let target = int <| ReadImmediate bytecode (state.ProgramCounter + 1) 2 (System.BitConverter.ToInt16)
                    Loop bytecode (JumpToPointer state (condition <> 0) target))
            | Instruction.STOP -> Ok <| None
            | _ -> Error "Undefined opcode"
    Loop bytecode state