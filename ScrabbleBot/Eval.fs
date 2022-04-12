module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H',4);('E',1);('L',1);('L',1);('O',1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b =
        a >>= fun a' ->
        b >>= fun b' ->
        ret (a' + b')

    let div a b =
        a >>= fun a' ->
        b >>= fun b' ->
        match b' with
        | 0 -> fail DivisionByZero
        | _ -> ret (a' / b')

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        match a with
        | N n         -> ret n
        | V s         -> lookup s
        | WL          -> wordLength
        | PV a        -> arithEval a >>= pointValue
        | Add (a, b)  -> add (arithEval a) (arithEval b)
        | Sub (a, b)  ->
            arithEval a >>= fun a' ->
            arithEval b >>= fun b' ->
            ret (a' - b')
        | Mul (a, b)  ->
            arithEval a >>= fun a' ->
            arithEval b >>= fun b' ->
            ret (a' * b')
        | Div (a, b)  -> div (arithEval a) (arithEval b)
        | Mod (a, b)  ->
            arithEval a >>= fun a' ->
            arithEval b >>= fun b' ->
            match b' with
            | 0 -> fail DivisionByZero
            | _ -> ret (a' % b')
        | CharToInt c ->
            charEval c >>= fun c' ->
            ret (int c')

    and charEval c : SM<char> =
        match c with
        | C c         -> ret c
        | CV a        -> arithEval a >>= characterValue
        | ToUpper c   ->
            charEval c >>= fun c' ->
            ret (System.Char.ToUpper c')
        | ToLower c   ->
            charEval c >>= fun c' ->
            ret (System.Char.ToLower c')
        | IntToChar a ->
            arithEval a >>= fun a' ->
            ret (char a')

    and boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false
        
        | AEq (a, b)  -> (* a = b *)
            arithEval a >>= fun a' ->
            arithEval b >>= fun b' ->
            ret (a' = b')
        | ALt (a, b)  -> (* a < b *)
            arithEval a >>= fun a' ->
            arithEval b >>= fun b' ->
            ret (a' < b')
        | Not b       -> (* not b *)
            boolEval b >>= fun b' ->
            ret (not b')
        | Conj (a, b) -> (* & *)
            boolEval a >>= fun a' ->
            boolEval b >>= fun b' ->
            ret (a' && b')
        
        | IsVowel c  ->
            charEval c >>= function
                           | 'A'|'E'|'I'|'O'|'U' -> ret true
                           | _                   -> ret false
        | IsLetter c ->
            charEval c >>= fun c' ->
            ret (System.Char.IsLetter c')
        | IsDigit  c ->
            charEval c >>= fun c' ->
            ret (System.Char.IsDigit c')


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    