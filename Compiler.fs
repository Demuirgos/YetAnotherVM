
module Language.Compiler
    open System
    open System.Collections.Generic
    open Language.Parser
    open Instructions
    open Utils

    type EmissionState = {
        SymbolTable : Dictionary<string, int>
        CallTable : Dictionary<string, int>
        mutable MemoryPointer : int
        mutable ErrorCount : int
    } with 
        static member Empty = 
            {
                MemoryPointer = 0
                SymbolTable = Dictionary<string, int>()
                CallTable = Dictionary<string, int>()
                ErrorCount = 0
            }

    let rec EmitBytecode bytecode (state : EmissionState) = 
        let rec EmitCodeSection idx codeSection = 
            if state.CallTable.Count = 0 then 
                state.CallTable["main"] <- 0

            let rec Loop instructions acc =
                match instructions with 
                | code::rest -> 
                    let rec handleSection code = 
                        match code with
                        | Return(exprOpt) when idx = 0 -> 
                            match exprOpt with 
                            | Some expr ->
                                Build {
                                    Inline ( expr |> handleSection |> _.Bytecode)
                                    Return
                                }
                            | _ -> 
                                Build {
                                    Return
                                }
                        | Return(exprOpt) when idx <> 0 -> 
                            match exprOpt with 
                            | Some expr ->
                                Build {
                                    Inline ( expr |> handleSection |> _.Bytecode)
                                    Retf
                                }
                            | _ -> 
                                Build {
                                    Retf
                                }
                        | Halt -> 
                            Build {
                                Stop
                            }
                        | Throw (error) -> 
                            let msgBytes = System.Text.Encoding.UTF8.GetBytes(error) 
                            let errorBytes = 
                                msgBytes
                                |> Array.chunkBySize 4
                                |> Array.rev
                                |> Array.map (fun chunk ->  [(byte)Instruction.PUSH; yield! chunk; yield! (Array.create (4 - Array.length chunk) 0uy) ])
                                |> Seq.concat |> Seq.toList
                            let errorId = sprintf "error%d" state.ErrorCount
                            state.ErrorCount <- state.ErrorCount + 1
                            state.SymbolTable[errorId] <- state.MemoryPointer
                            state.MemoryPointer <- state.MemoryPointer + msgBytes.Length
                            Build {
                                Inline errorBytes
                                Store (int16 state.SymbolTable[errorId]) (int16 (msgBytes.Length / 4))
                                Fail (int16 state.SymbolTable[errorId]) (int16 (msgBytes.Length))
                            }
                        | FunctionDecl(name, args, hasReturnValue, body) as funDec -> 
                            state.CallTable[name] <- state.CallTable.Count
                            Build {
                                Defer (Build {
                                    Signature (byte <| List.length args, if hasReturnValue then 1uy else 0uy)
                                    Inline (
                                        args 
                                        |> List.rev
                                        |> List.map ( fun arg -> 
                                            let mangledName = sprintf "%d%s" state.CallTable[name] arg
                                            state.SymbolTable[mangledName] <- state.MemoryPointer
                                            state.MemoryPointer <- state.MemoryPointer + 4
                                            (Build {
                                                Store (int16 state.SymbolTable[mangledName]) 1s
                                            }).Bytecode) 
                                        |> List.concat
                                    )

                                    Inline (EmitCodeSection (state.CallTable[name]) body |> _.Bytecode)
                                }).Bytecode
                            }
                        | VariableDecl(name, valueOpt) -> 
                            let mangledName = sprintf "%d%s" idx name
                            state.SymbolTable[mangledName] <- state.MemoryPointer
                            state.MemoryPointer <- state.MemoryPointer + 4
                            match valueOpt with 
                            | Some(value) -> 
                                Build {
                                    Inline (handleSection value |> _.Bytecode)
                                    Store (int16 state.SymbolTable[mangledName]) 1s
                                }
                            | None ->
                                Build {
                                    Push 0
                                    Store (int16 state.SymbolTable[mangledName]) 1s
                                } 
                        | WhileStatement(cond, body) -> 
                            let cond = (handleSection cond).Bytecode
                            let body = EmitCodeSection idx body |> _.Bytecode
                            Build {
                                Inline cond
                                Not
                                Cjump (int16 (3 + List.length body))
                                Inline body
                                Jump (-3s - int16(List.length body) - 3s - 1s - int16(List.length cond))
                            }
                        | IfStatement(cond, body, altBody) -> 
                            let cond = (handleSection cond).Bytecode
                            let body = EmitCodeSection idx body |> _.Bytecode
                            let elseBody = 
                                match altBody with 
                                | Some(elseBody) -> EmitCodeSection idx elseBody |> _.Bytecode
                                | None -> []
                            Build {
                                Inline cond
                                Not
                                Cjump (3s + (int16 <| List.length body))
                                Inline body
                                Jump (int16 <| List.length elseBody)
                                Inline elseBody
                            }
                        | VariableAssign(name, value) ->
                            let mangledName = sprintf "%d%s" idx name
                            Build {
                                Inline ((handleSection value).Bytecode)
                                Store (int16 state.SymbolTable[mangledName]) 1s
                            }
                        | ExpressionNode(expr) -> 
                            match expr with
                            | Paren expr -> 
                                Build {
                                    Inline (handleSection expr).Bytecode
                                } 
                            | Single value -> 
                                Build {
                                    Push value
                                }
                            | Variable name -> 
                                let mangledName = sprintf "%d%s" idx name
                                Build {
                                    Load (int16 state.SymbolTable[mangledName]) 1s
                                }
                            | Call(name, args) -> 
                                match name with 
                                | "read" ->
                                    Build {
                                        Read
                                    }
                                | _ -> 
                                    Build {
                                        Inline (args |> List.map (handleSection >> _.Bytecode) |> List.concat)
                                        Call (int16 state.CallTable[name])
                                    }
                            | Binary(lhs, op, rhs) -> 
                                let instruction = 
                                    match op with 
                                    | '*' -> Instruction.MUL
                                    | '+' -> Instruction.ADD
                                    | '-' -> Instruction.SUB
                                    | '/' -> Instruction.DIV
                                    | '&' -> Instruction.AND
                                    | '|' -> Instruction.OR
                                    | '%' -> Instruction.MOD
                                    | '^' -> Instruction.EXP
                                    | '<' -> Instruction.LT
                                    | '>' -> Instruction.GT
                                    | '=' -> Instruction.EQ
                                Build {
                                    Inline (handleSection rhs |> _.Bytecode)
                                    Inline (handleSection lhs |> _.Bytecode) 
                                    Op instruction 
                                }
                            | Unary(op, rhs) -> 
                                let instruction = 
                                    match op with 
                                    | '!' -> Instruction.MOD
                                    | '~' -> Instruction.NEG
                                    | '-' -> Instruction.NEG
                                Build {
                                    Inline (handleSection rhs |> _.Bytecode)
                                    Op instruction 
                                }
                    Loop rest ((handleSection code)::acc)
                | [] -> List.rev acc 
            let result = 
                Loop codeSection [] |> List.fold (fun st item -> {
                    st with  Bytecode = st.Bytecode@item.Bytecode
                             Deferred = st.Deferred@item.Deferred

                }) { Bytecode = []; Deferred = []; }
            result
        let bytecodeBuilderState = EmitCodeSection 0 bytecode
        [0uy::0uy::bytecodeBuilderState.Bytecode]@bytecodeBuilderState.Deferred