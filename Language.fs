
module Language

    open System
    open parsec.Parsec.Primitives
    open parsec.Parsec

    type Expression = 
        | Single of int
        | Binary of Grammar * char * Grammar
        | Call of string * Grammar list  
    and Grammar = 
        | VariableDecl   of string * Grammar option
        | FunctionDecl   of string * (string list) * Grammar list
        | IfStatement    of Grammar * Grammar list * Grammar list option
        | WhileStatement of Grammar * Grammar list
        | ExpressionNode of Expression
    (*
        var name = value;

        fun name(arg1, arg2...) {
            // instructions
            return 0 | empty;
        }
        
        if(condition) {
            // instructions;
        } else {

        }

        while(condition) {
            // instructions;
        }
    *) 

    let ParseProgram = 
        let digits = ['0'..'9'] 
        let operators = ['+'; '-'; '*'; '/'; '&'; '%'; '|'; '^'; '~'] 
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
            
            let mutable poolParser = [parseValue]
            if includeBin then 
                poolParser <-  parseBinary::poolParser
            if includeFun then 
                poolParser <-  parseFunctionCall::poolParser
             
            (choice poolParser) <?> "Expression" |>>  ExpressionNode 
        and parseVariableDecl = 
            Parser {
                let pVar = allOf ['v'; 'a'; 'r']
                let pEq = expect '='
                return! pVar >>. pSpaces >>. pName .>> pSpaces .>>. option(pEq >>. pSpaces >>. (parseExpression true true)) .>> expect ';'
            } <?> "VarDeclStmt" |>> (fun (a, b) -> (fromArrToStr a,b) |> VariableDecl)
        and parseIfElse = 
            Parser {
                let pIf = allOf ['i'; 'f']
                let pElse = allOf ['e'; 'l'; 's'; 'e']
                return! pIf   .>> pSpaces >>. pLeftParen .>> pSpaces >>. (parseExpression true true) .>> pSpaces .>> pRightParen 
                              .>> pSpaces .>> pLeftCurly .>> pSpaces .>>. many 0 parseInstruction .>> pSpaces .>> pRightCurly .>> pSpaces
                    .>>. option (pElse >>. pSpaces >>. pLeftCurly >>. pSpaces >>. many 0 parseInstruction .>> pSpaces .>> pRightCurly)
            } <?> "IfElseStmt" |>> (fun ((cond, happyPath), sadPath) -> (cond, happyPath, sadPath) |> IfStatement)
        and parseWhile = 
            Parser {
                let pWhile = allOf ['w'; 'h'; 'i'; 'l'; 'e']
                return! pWhile .>> pSpaces >>. pLeftParen .>> pSpaces >>. (parseExpression true true) .>> pSpaces .>> pRightParen 
                               .>> pSpaces .>> pLeftCurly .>> pSpaces .>>. many 0 parseInstruction .>> pSpaces .>> pRightCurly 
            } <?> "WhileStmt" |>> (fun (cond, body)-> (cond, body) |> WhileStatement)
        and parseFunctionDec = 
            Parser {
                let pFun = allOf ['f'; 'u'; 'n']
                let pArgs = between pLeftParen (many 0 pName) pRightParen
                return! pFun >>. pSpaces >>. pName .>> pSpaces .>>. pArgs 
                        .>> pSpaces .>> pLeftCurly 
                        .>>. many 0 parseInstruction 
                        .>> pSpaces .>> pRightCurly             
            } <?> "VarDeclStmt" |>> (fun ((name, argsList), body) -> (fromArrToStr name, List.map fromArrToStr argsList, body) |> FunctionDecl)
        
        and parseInstruction = 
            parseVariableDecl <|> parseIfElse <|> parseWhile
        many 0 parseInstruction
