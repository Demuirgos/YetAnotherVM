
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

            let returnBufferPrefix : int16 = 4s

            let rec Loop instructions acc =
                match instructions with 
                | code::rest -> 
                    let rec handleSection code = 
                        match code with
                        | EffectCall(procCall) -> 
                            let bodyBc = handleSection procCall |> _.Bytecode
                            Build {
                                Inline bodyBc
                            }
                        | Return(exprOpt) when idx = 0 -> 
                            match exprOpt with 
                            | Some expr ->
                                Build {
                                    Inline ( expr |> handleSection |> _.Bytecode)
                                    Store 0uy 0s
                                    Return 0s returnBufferPrefix
                                }
                            | _ -> 
                                Build {
                                    Stop
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
                                                Store 0uy (returnBufferPrefix + int16 state.SymbolTable[mangledName])
                                            }).Bytecode) 
                                        |> List.concat
                                    )

                                    Inline (EmitCodeSection (state.CallTable[name]) body |> _.Bytecode)
                                }).Bytecode
                            }
                        | VariableDecl(name, sizeOpt, valueOpt) -> 
                            let (isDynamic, size, DefValue) = 
                                match sizeOpt with 
                                | Some(size) -> (0uy, int16 size, Array.create size 0) 
                                | None -> (0uy, 1s, [||])
                            let mangledName = sprintf "%d%s" idx name
                            state.SymbolTable[mangledName] <- state.MemoryPointer
                            state.MemoryPointer <- state.MemoryPointer + (4 * int size)
                            match valueOpt with 
                            | Some(value) -> 
                                Build {
                                    Inline (handleSection value |> _.Bytecode)
                                    Inline (
                                        [
                                            for offset in 0..int size - 1 do 
                                                yield Build {
                                                    Store isDynamic (returnBufferPrefix + int16 state.SymbolTable[mangledName] + 4s * int16 offset)
                                                } |> _.Bytecode
                                        ] |> List.concat
                                    )

                                }
                            | None ->
                                Build {
                                    Inline (
                                        DefValue 
                                        |> Array.mapi (fun offset _ -> 
                                                Build {
                                                    Push 0
                                                    Store isDynamic (returnBufferPrefix + int16 state.SymbolTable[mangledName] + 4s * int16 offset)
                                                } |> _.Bytecode
                                            )
                                        |> List.concat
                                    )
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
                        | VariableAssign(name, indexOpt, value) ->
                            let mangledName = sprintf "%d%s" idx name
                            match indexOpt with 
                            | Some(indexNode) -> 
                                Build {
                                    Inline (handleSection value).Bytecode
                                    Inline (handleSection indexNode).Bytecode
                                    Push 04
                                    Mul
                                    Store 1uy (returnBufferPrefix + int16 state.SymbolTable[mangledName]) 
                                }
                            | None -> 
                                Build {
                                    Inline ((handleSection value).Bytecode)
                                    Store 0uy (returnBufferPrefix + int16 state.SymbolTable[mangledName]) 
                                }
                        | ExpressionNode(expr) -> 
                            match expr with
                            | Indexer(name, indexer) -> 
                                let mangledName = sprintf "%d%s" idx name
                                let indexerBc = handleSection indexer |> _.Bytecode
                                Build {
                                    Inline indexerBc
                                    Push 04
                                    Mul 
                                    Load 1uy (returnBufferPrefix + int16 state.SymbolTable[mangledName]) 
                                }
                            | List(items) -> 
                                let itemsBc = 
                                    items
                                    |>  List.rev
                                    |>  List.map (handleSection >> _.Bytecode)
                                    |>  List.concat
                                Build {
                                    Inline itemsBc
                                }
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
                                    Load 0uy (returnBufferPrefix + int16 state.SymbolTable[mangledName]) 
                                }
                            | Call(name, args) -> 
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
                        | Comment(_) -> 
                                Build {
                                    Empty
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