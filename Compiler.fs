
module Language.Compiler
    open System
    open System.Collections.Generic
    open Language.Parser
    open Instructions

    let rec EmitBytecode bytecode (state : Dictionary<_,_> * Dictionary<_,_>) = 
        let symbolTable, callTable = state
        let rec EmitCodeSection idx codeSection = 
            if callTable.Count = 0 then 
                callTable["main"] <- 0

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
                        | Throw (_) -> 
                            Build {
                                Fail
                            }
                        | FunctionDecl(name, args, hasReturnValue, body) as funDec -> 
                            callTable[name] <- callTable.Count
                            Build {
                                Defer (Build {
                                    Signature (byte <| List.length args, if hasReturnValue then 1uy else 0uy)
                                    Inline (
                                        args 
                                        |> List.rev
                                        |> List.map ( fun arg -> 
                                            let mangledName = sprintf "%d%s" callTable[name] arg
                                            symbolTable[mangledName] <- symbolTable.Count * 4
                                            (Build {
                                                Store (int16 symbolTable[mangledName])
                                            }).Bytecode) 
                                        |> List.concat
                                    )

                                    Inline (EmitCodeSection (callTable[name]) body |> _.Bytecode)
                                }).Bytecode
                            }
                        | VariableDecl(name, valueOpt) -> 
                            let mangledName = sprintf "%d%s" idx name
                            symbolTable[mangledName] <- symbolTable.Count * 4
                            match valueOpt with 
                            | Some(value) -> 
                                Build {
                                    Inline (handleSection value |> _.Bytecode)
                                    Store (int16 symbolTable[mangledName])
                                }
                            | None ->
                                Build {
                                    Push 0
                                    Store (int16 symbolTable[mangledName])
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
                                Store (int16 symbolTable[mangledName])
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
                                    Load (int16 symbolTable[mangledName])
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
                                        Call (int16 callTable[name])
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
                }) { Bytecode = []; Deferred = [] }
            result
        let bytecodeBuilderState = EmitCodeSection 0 bytecode
        [0uy::0uy::bytecodeBuilderState.Bytecode]@bytecodeBuilderState.Deferred