module Outils
open Instructions

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

let ExtractCodeSections bytecode transform = 
    let rec Loop transform bytecode offset idx acc = 
        let ExtractCodeSectionsPartial = Loop transform
        let count = int <| Seq.item 0 bytecode 
        if idx >= count then List.rev acc 
        else 
            let sectionInput = Seq.item (1 + (idx * 4)) bytecode 
            let sectionOutput = Seq.item (2 + (idx * 4)) bytecode 
            let sectionSize = int <| ReadImmediate bytecode (3 + (idx * 4)) 2 System.BitConverter.ToInt16
            let codeSection = Seq.skip (1 + count * 4 + offset) bytecode
            let value = transform idx sectionInput sectionOutput sectionSize offset codeSection
            ExtractCodeSectionsPartial bytecode (offset + sectionSize) (idx + 1) (value::acc)
    Loop transform bytecode 0 0 []

let BytecodeToMnemonic bytecode = 
    let rec handleSection (sectionCode : byte seq) idx acc = 
        if idx >= Seq.length sectionCode then System.String.Join("\n", (List.rev acc))
        else 
            let instruction : Instruction = LanguagePrimitives.EnumOfValue (int <| (Seq.item idx sectionCode)) 
            let immediateCount = int (Instructions.GetMetadata instruction).ImmediateArgument
            let argument = 
                (sectionCode |> Seq.skip (idx + 1) |> Seq.take immediateCount |> Seq.rev |> Seq.toArray)
                |>  if immediateCount = 2 
                    then System.BitConverter.ToInt16 >> int >> Some
                    else if immediateCount = 4 
                         then System.BitConverter.ToInt32 >> Some
                         else ignore >> fun _ -> None

            handleSection sectionCode (idx + 1 + immediateCount) ((sprintf "%A: %A" instruction argument)::acc)
    let functions = ExtractCodeSections bytecode (fun index inputCount outputCount size ptr code -> 
            index, handleSection code 0 []
        ) 
    Map functions
    
let MnemonicToBytecode bytecodeStr = failwith "not implemented"