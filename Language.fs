
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
    and Grammar = 
        | VariableDecl   of string * Grammar option
        | VariableAssign of string * Grammar 
        | FunctionDecl   of string * (string list) * bool * Grammar list
        | IfStatement    of Grammar * Grammar list * Grammar list option
        | WhileStatement of Grammar * Grammar list
        | ExpressionNode of Expression
        | Return         of Grammar option
        | Halt

    let ParseProgram = 
        let digits = ['0'..'9'] 
        let operators = ['+'; '-'; '*'; '/'; '&'; '%'; '|'; '^'; '~'; '='; '<'; '>'; '!'] 
        let chars = 
            "abcdefghijklmnopqrstuvwxyz"
            |> fun a -> sprintf "%s%s" a (a.ToUpper())
            |> fun s -> s.ToCharArray()
            |> Array.toList
        
        let pLeftParen = expect '('
        let pRightParen = expect ')'
        let pLeftCurly = expect '{'
        let pRightCurly = expect '}'
        let pName = many 1 (anyOf chars)

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
                    let pName = many 1 (anyOf chars)
                    let pArgs = between pLeftParen (separate1By (parseExpression true true) (expect ',')) pRightParen                    
                    return! pName .>>. pArgs 
                } <?> "FuncCall" |>> (fun (a, b) -> (fromArrToStr a,b) |> Call)
            and parseBinary = 
                Parser {
                    let pOperator = anyOf operators
                    return! (parseExpression false true) .>> pSpaces .>>. pOperator .>> pSpaces .>>. (parseExpression true true) 
                } <?> "BinOp" |>> (fun ((lhs, op), rhs) -> (lhs, op, rhs) |> Binary)
            
            let mutable poolParser = [parseValue; parseBoolean; parseVar]
            if includeBin then 
                poolParser <-  parseBinary::poolParser
            if includeFun then 
                poolParser <-  parseFunctionCall::poolParser
             
            (choice poolParser) <?> "Expression" |>>  ExpressionNode 
        and parseInstruction isDeep = 
            let defaultParser = parseReturn <|> parseHalt <|> parseVariableAssignment <|> parseVariableDecl <|> parseIfElse <|> parseWhile
            if isDeep then defaultParser 
            else parseFunctionDec <|> defaultParser 
        and parseVariableDecl = 
            Parser {
                let pVar = allOf ['v'; 'a'; 'r']
                let pEq = expect '='
                return! pVar >>. pSpaces >>. pName .>> pSpaces .>>. option(pEq >>. pSpaces >>. (parseExpression true true)) .>> expect ';'
            } <?> "VarDeclStmt" |>> (fun (a, b) -> (fromArrToStr a,b) |> VariableDecl)
        and parseVariableAssignment = 
            Parser {
                let pArrow = allOf ['<'; '-']
                return! pName .>> pSpaces .>> pArrow .>> pSpaces .>>. (parseExpression true true) .>> expect ';'
            } <?> "VarAssignment" |>> (fun (a, b) -> (fromArrToStr a,b) |> VariableAssign)
        and parseIfElse = 
            Parser {
                let pIf = allOf ['i'; 'f']
                let pElse = allOf ['e'; 'l'; 's'; 'e']
                return! pIf   .>> pSpaces >>. pLeftParen .>> pSpaces >>. (parseExpression true true) .>> pSpaces .>> pRightParen 
                              .>> pSpaces .>> pLeftCurly .>> pSpaces .>>. separate1By (parseInstruction true) pSpaces .>> pSpaces .>> pRightCurly .>> pSpaces
                    .>>. option (pElse >>. pSpaces >>. pLeftCurly >>. pSpaces >>. many 0 (parseInstruction true) .>> pSpaces .>> pRightCurly)
            } <?> "IfElseStmt" |>> (fun ((cond, happyPath), sadPath) -> (cond, happyPath, sadPath) |> IfStatement)
        and parseWhile = 
            Parser {
                let pWhile = allOf ['w'; 'h'; 'i'; 'l'; 'e']
                return! pWhile .>> pSpaces >>. pLeftParen .>> pSpaces >>. (parseExpression true true) .>> pSpaces .>> pRightParen 
                               .>> pSpaces .>> pLeftCurly .>> pSpaces .>>. separate1By (parseInstruction true) pSpaces .>> pSpaces .>> pRightCurly 
            } <?> "WhileStmt" |>> (fun (cond, body)-> (cond, body) |> WhileStatement)
        and parseFunctionDec = 
            Parser {
                let pFun = allOf ['f'; 'u'; 'n']
                let pProc = allOf ['p'; 'r'; 'o'; 'c']
                let pArgs = between pLeftParen (separate1By pName (expect ',')) pRightParen
                return! (pFun <|> pProc) .>> pSpaces .>>. pName .>> pSpaces .>>. pArgs 
                        .>> pSpaces .>> pLeftCurly 
                        .>> pSpaces .>>. separate1By (parseInstruction true) pSpaces
                        .>> pSpaces .>> pRightCurly             
            } <?> "FunDeclStmt" |>> (fun (((keyword, name), argsList), body) -> (fromArrToStr name, List.map fromArrToStr argsList, (fromArrToStr keyword) = "fun", body) |> FunctionDecl)
        and parseHalt = 
            Parser {
                return! allOf (List.ofSeq "halt") .>> expect ';'
            } <?> "HaltStatement" |>> fun _ -> Halt
        and parseReturn = 
            Parser {
                return! allOf (List.ofSeq "return") >>. pSpaces >>. option (parseExpression true true) .>> expect ';'
            } <?> "ReturnStatement" |>> fun expr -> Return expr

        pSpaces >>. separate1By (parseInstruction false) pSpaces
