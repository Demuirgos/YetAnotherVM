module Utils
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

let GenerateHeader (sections : byte list seq) = 
    let sectionsCount = byte <| Seq.length sections
    let sectionIOandSize (section: byte list) = 
        let inputCount = section[0]
        let outputCount = section[1]
        let size = 
            let sizeBytes = int16((List.length section) - 2) |> System.BitConverter.GetBytes
            if System.BitConverter.IsLittleEndian then 
                Array.rev sizeBytes 
            else sizeBytes
        [inputCount; outputCount; yield! size] |> Seq.ofList

    let sectionBody (section: byte list) = List.skip 2 section


    seq {
        yield sectionsCount
        yield! sections |> Seq.map sectionIOandSize |> Seq.concat
        yield! sections |> Seq.map sectionBody |> List.concat 
    }

let ExtractCodeSections bytecode transform = 
    let rec Loop transform bytecode offset idx acc = 
        let ExtractCodeSectionsPartial = Loop transform
        let count = int <| Seq.item 0 bytecode 
        if idx >= count then List.rev acc 
        else 
            let sectionInput = Seq.item (1 + (idx * 4)) bytecode 
            let sectionOutput = Seq.item (2 + (idx * 4)) bytecode 
            let sectionSize = int <| ReadImmediate bytecode (3 + (idx * 4)) 2 System.BitConverter.ToInt16
            let codeSection = 
                bytecode 
                |> Seq.skip (1 + count * 4 + offset)  
                |> Seq.take sectionSize
            let value = transform idx sectionInput sectionOutput sectionSize offset codeSection
            ExtractCodeSectionsPartial bytecode (offset + sectionSize) (idx + 1) (value::acc)
    Loop transform bytecode 0 0 []

let BytecodeToMnemonic bytecode = 
    let rec handleSection (sectionCode : byte seq) idx acc = 
        if idx >= Seq.length sectionCode then System.String.Join("\n\t", (List.rev acc))
        else 
            let instruction : Instruction = LanguagePrimitives.EnumOfValue (int <| (Seq.item idx sectionCode)) 
            let immediateCount = int (Instructions.GetMetadata instruction).ImmediateArgument
            let argument = 
                (sectionCode |> Seq.skip (idx + 1) |> Seq.take immediateCount |> Seq.rev |> Seq.toArray)
                |>  sprintf "%A"
            handleSection sectionCode (idx + 1 + immediateCount) ((sprintf "%A\t%s" instruction argument)::acc)
    let functions = ExtractCodeSections bytecode (fun index inputCount outputCount size ptr code -> 
            index, (inputCount, outputCount), handleSection code 0 []
        ) 
    
    System.String.Join("\n", 
        functions
        |> List.map (fun (index, (ins, outs), body) -> sprintf "%d:%d, %d\t%s" index ins outs body)
    )