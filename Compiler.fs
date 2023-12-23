
module Language.Compiler
    open System
    open System.Collections.Generic
    open Language.Parser

    let rec EmitBytecode code = 
        let symbolTable = Dictionary<_,_>()
        match code with 
        | VariableDecl(name, valueOpt) -> 
            symbolTable[name] = symbolTable.Count
            match valueOpt with 
            | Some(value) -> 
                
                (EmitBytecode value)