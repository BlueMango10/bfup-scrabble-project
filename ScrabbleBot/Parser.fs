module internal Parser

    open Eval
    open StateMonad

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    open ScrabbleUtil

    (* * Green * *)
    (* Exercise 7.1 *)
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "itLetter"
    let pIsVowel    = pstring "itVowel"

    let pif         = pstring "if"
    let pthen       = pstring "then"
    let pelse       = pstring "else"
    let pwhile      = pstring "while"
    let pdo         = pstring "do"
    let pdeclare    = pstring "declare"

    (* Assignment 7.2 *)
    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    (* Assignment 7.3 *)
    let (.>*>.) a b = a .>> spaces .>>. b
    let (.>*>)  a b = a .>> (spaces >>. b)
    let (>*>.)  a b = a .>> spaces >>. b

    (* Assignment 7.4 *)
    let parenthesise p = pstring "(" >*>. p .>*> pstring ")"
    let parenthesisecurly p = pstring "{" >*>. p .>*> pstring "}"

    (* Assignment 7.5 *)
    let pid = (pchar '_' <|> pletter) .>>. many (palphanumeric <|> pchar '_')|>> fun (a, b) ->
        System.String.Concat(a::b)

    (* Assignment 7.6*)
    let unop op a = op >*>. a

    (* Assignment 7.7 *)
    let binop op a b = a .>*> op .>*>. b

    (* Assignment 7.8 *)
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    // Terms
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"

    // Prods
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"

    // Atoms
    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid    |>> V <?> "Variable"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> (fun a -> Mul(N(-1), a)) <?> "Negation"
    let PVParse  = unop (pstring "pointValue") ParParse |>> PV <?> "Point Value"

    let AexpParse = TermParse 

    (* Assignment 7.9 *)
    let CexpParse, cref = createParserForwardedToRef<cExp>()

    // Cexp
    let CParse = pchar ''' >>. satisfy (fun _ -> true) .>> pchar ''' |>> C <?> "Char"
    let CVParse = unop (pstring "charValue") ParParse |>> CV <?> "Character Value"
    let IntToCharParse = unop (pstring "intToChar") ParParse |>> IntToChar <?> "intToChar"
    let ToUpperParse = unop (pstring "toUpper") (parenthesise CexpParse) |>> ToUpper <?> "toUpper"
    let ToLowerParse = unop (pstring "toLower") (parenthesise CexpParse) |>> ToLower <?> "toLower"

    // This is Aexp Atom
    let CharToIntParse = unop (pstring "charToInt") (parenthesise CexpParse) |>> CharToInt <?> "charToInt"


    do tref := choice [AddParse; SubParse; ProdParse]
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    do aref := choice [CharToIntParse; NegParse; NParse; ParParse; PVParse; VParse]
    do cref := choice [IntToCharParse; ToUpperParse; ToLowerParse; CVParse; CParse]

    (* * Yellow * *)
    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}

