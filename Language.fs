
module Language.Parser

    open System
    open parsec.Parsec.Primitives
    open parsec.Parsec

    type Expression = 
        | Variable of string
        | Single of int
        | Binary of Grammar * char * Grammar
        | Unary of char * Grammar
        | Call of string * Grammar list
        | List of Grammar list
        | Indexer of string * Grammar
        | Paren of Grammar  
    and Grammar = 
        | VariableDecl   of string * int option * Grammar option
        | VariableAssign of string * Grammar option * Grammar 
        | FunctionDecl   of string * (string list) * bool * Grammar list
        | IfStatement    of Grammar * Grammar list * Grammar list option
        | WhileStatement of Grammar * Grammar list
        | ExpressionNode of Expression
        | EffectCall     of Grammar
        | Return         of Grammar option
        | Comment        of string

    let ParseProgram = 
        let digits = ['0'..'9'] 
        let operators = ['+'; '-'; '*'; '/'; '&'; '%'; '|'; '^'; '~'; '='; '<'; '>'; '!'] 
        let alphabets = ['a'..'z']@['A'..'Z']
        let special = [' '; '_'; '('; ')'; '{'; '}'; '['; ']'; '@'; '`'; ';'; ','; '.'; '|'; '$']
        let spaces = ['\n'; '\t'; '\r']

        let chars = alphabets@special@operators@digits@spaces

        let pLeftParen = expect '('
        let pRightParen = expect ')'
        let pLeftCurly = expect '{'
        let pRightCurly = expect '}'
        let pLeftBra = expect '['
        let pRightKet = expect ']'
        let pBraket = allOf ['['; ']']

        let pName = many 1 (anyOf alphabets)

        let fromArrToStr str = str |> List.toArray |> (fun s -> System.String s)

        let rec parseExpression includeBin includeFun =
            let rec parseValue = 
                Parser {
                    return! many 1 (anyOf digits)
                } <?> "Value" |>> fun v -> (Int32.Parse (fromArrToStr v)) |> Single 
            and parseVar = 
                Parser {
                    return! pName
                } <?> "Variable" |>> (fromArrToStr >> Variable) 
            and parseBoolean = 
                Parser {
                    let pTrue = allOf (List.ofSeq "true")
                    let pFalse = allOf (List.ofSeq "false")
                    return! pTrue <|> pFalse
                } <?> "Boolean" |>> fun v -> (if Boolean.Parse (fromArrToStr v) then 1 else 0) |> Single 
            and parseFunctionCall = 
                Parser {
                    let pName = many 1 (anyOf alphabets)
                    let pArgs = between pLeftParen (separateBy (parseExpression true true) (pSpaces >>. (expect ',') >>. pSpaces)) pRightParen                    
                    return! pName .>>. pArgs 
                } <?> "FuncCall" |>> (fun (a, b) -> (fromArrToStr a,b) |> Call)
            and parseBinary = 
                Parser {
                    let pOperator = anyOf operators
                    return! (parseExpression false true) .>> pSpaces .>>. pOperator .>> pSpaces .>>. (parseExpression true true) 
                } <?> "BinOp" |>> (fun ((lhs, op), rhs) -> (lhs, op, rhs) |> Binary)
            and parseParen = 
                Parser {
                    return! pLeftParen >>. parseExpression true true .>> pRightParen 
                } <?> "BinOp" |>> Paren
            and parseList = 
                Parser {
                    return! pLeftBra >>. separateBy (parseExpression true true) (option (pSpaces >>. (expect ',') >>. pSpaces)) .>> pRightKet 
                } <?> "List" |>> List
            and parseIndex = 
                Parser {
                    return! pName .>> pLeftBra .>>. parseExpression true true .>> pRightKet 
                } <?> "Indexer" |>> fun (nameCharsList, indexer) -> (fromArrToStr nameCharsList, indexer) |> Indexer
            let mutable poolParser = [parseIndex; parseList; parseParen; parseValue; parseBoolean; parseVar]
            if includeBin then 
                poolParser <-  parseBinary::poolParser
            if includeFun then 
                poolParser <-  parseFunctionCall::poolParser
             
            (choice poolParser) <?> "Expression" |>>  ExpressionNode 
        and parseInstruction isDeep = 
            let defaultParser = parseEffectCall <|> parseComment <|> parseReturn <|> parseVariableAssignment <|> parseVariableDecl <|> parseIfElse <|> parseWhile
            if isDeep then defaultParser 
            else parseFunctionDec <|> defaultParser 
        and parseEffectCall = 
            Parser {
                let pDo = allOf ['d'; 'o']
                return! pDo >>. pSpaces >>. (parseExpression false true) .>> expect ';'
            } <?> "EffectCall" |>> EffectCall
        and parseVariableDecl = 
            let parseFromOption option = 
                match option with 
                | Some(value) -> Some(Int32.Parse <| (fromArrToStr <| value))
                | _ -> None
            Parser {
                let pVar = allOf ['v'; 'a'; 'r']
                let pEq = expect '='
                return! pVar >>. pSpaces >>. pName .>>. option (pLeftBra >>. many 1  (anyOf digits) .>> pRightKet) .>> pSpaces .>>. option(pEq >>. pSpaces >>. (parseExpression true true)) .>> expect ';'
            } <?> "VarDeclStmt" |>> (fun ((name, sizeOpt), value) -> (fromArrToStr name, parseFromOption sizeOpt ,value) |> VariableDecl)
        and parseVariableAssignment = 
            Parser {
                let pArrow = allOf ['<'; '-']
                return! pName .>>. option (pLeftBra >>. parseExpression true true .>> pRightKet) .>> pSpaces .>> pArrow .>> pSpaces .>>. (parseExpression true true) .>> expect ';'
            } <?> "VarAssignment" |>> (fun ((name, indexerOpt), value) -> (fromArrToStr name, indexerOpt,  value) |> VariableAssign)
        and parseIfElse = 
            Parser {
                let pIf = allOf ['i'; 'f']
                let pElse = allOf ['e'; 'l'; 's'; 'e']
                return! pIf   .>> pSpaces >>. pLeftParen .>> pSpaces >>. (parseExpression true true) .>> pSpaces .>> pRightParen 
                              .>> pSpaces .>> pLeftCurly .>> pSpaces .>>. separateBy (parseInstruction true) pSpaces .>> pSpaces .>> pRightCurly .>> pSpaces
                    .>>. option (pElse >>. pSpaces >>. pLeftCurly >>. pSpaces >>. separateBy (parseInstruction true) pSpaces .>> pSpaces .>> pRightCurly)
            } <?> "IfElseStmt" |>> (fun ((cond, happyPath), sadPath) -> (cond, happyPath, sadPath) |> IfStatement)
        and parseWhile = 
            Parser {
                let pWhile = allOf ['w'; 'h'; 'i'; 'l'; 'e']
                return! pWhile .>> pSpaces >>. pLeftParen .>> pSpaces >>. (parseExpression true true) .>> pSpaces .>> pRightParen 
                               .>> pSpaces .>> pLeftCurly .>> pSpaces .>>. separateBy (parseInstruction true) pSpaces .>> pSpaces .>> pRightCurly 
            } <?> "WhileStmt" |>> (fun (cond, body)-> (cond, body) |> WhileStatement)
        and parseFunctionDec = 
            Parser {
                let pFun = allOf ['f'; 'u'; 'n']
                let pProc = allOf ['p'; 'r'; 'o'; 'c']
                let pArgs = between pLeftParen (separateBy pName (pSpaces >>. (expect ',') >>. pSpaces)) pRightParen
                return! (pFun <|> pProc) .>> pSpaces .>>. pName .>> pSpaces .>>. pArgs 
                        .>> pSpaces .>> pLeftCurly 
                        .>> pSpaces .>>. separateBy (parseInstruction true) pSpaces
                        .>> pSpaces .>> pRightCurly             
            } <?> "FunDeclStmt" |>> (fun (((keyword, name), argsList), body) -> (fromArrToStr name, List.map fromArrToStr argsList, (fromArrToStr keyword) = "fun", body) |> FunctionDecl)
        and parseReturn = 
            Parser {
                return! allOf (List.ofSeq "return") >>. pSpaces >>. option (parseExpression true true) .>> expect ';'
            } <?> "ReturnStatement" |>> fun expr -> Return expr
        and parseComment = 
                between (expect '\\') (many 0 (anyOf chars)) (expect '\\') 
                <?> "CommentStatement" |>> (fromArrToStr >> Comment)
        pSpaces >>. separateBy (parseInstruction false) pSpaces
