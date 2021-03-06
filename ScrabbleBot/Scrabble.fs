namespace CIIVLBot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand (pieces : Map<uint32,tile>) hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board          : Parser.board
        dict           : Dictionary.Dict
        playerNumber   : uint32
        hand           : MultiSet.MultiSet<uint32>
        pieces         : Map<coord, (char * int)>
        totalPlayers   : uint32
        turn           : uint32
        droppedPlayers : Set<uint32> // Players who has left the game
    }

    let mkState b d pn h p tp t dp = {
        board = b;
        dict = d;
        playerNumber = pn;
        hand = h;
        pieces = p;
        totalPlayers = tp;
        turn = t;
        droppedPlayers = dp;
    }

    let board st          = st.board
    let dict st           = st.dict
    let playerNumber st   = st.playerNumber
    let hand st           = st.hand
    let pieces st         = st.pieces
    let totalPlayers st   = st.totalPlayers
    let turn st           = st.turn
    let droppedPlayers st = st.droppedPlayers

    let withBoard b st           = {st with board = b}
    let withDict d st            = {st with dict = d}
    let withPlayerNumber pn st   = {st with playerNumber = pn}
    let withHand h st            = {st with hand = h}
    let withPieces p st          = {st with pieces = p}
    let withTotalPlayers tp st   = {st with totalPlayers = tp}
    let withTurn t st            = {st with turn = t}
    let withDroppedPlayers dp st = {st with droppedPlayers = dp}

module Scrabble =
    open System.Threading
    type letter = uint32 * (char * int) // Used to represent both the tile and the id of a piece
    type internal msetHand = MultiSet.MultiSet<uint32>
    type internal explHand = MultiSet.MultiSet<letter>
    let PREFERRED_WORD_LENGTH = 3

    let playGame cstream (pieces : Map<uint32,tile>) (st : State.state) =

        let nextTurn t st =
            let rec aux t' =
                match (t' % (State.totalPlayers st)) + 1u with // IDs start from 1
                | t'' when Set.contains t'' (State.droppedPlayers st) -> aux t''
                | t'' -> t''
            st |> State.withTurn (aux t)


        let placePieces (co: coord, (_,(piece: char * int))) st =
            st |> State.withPieces (Map.add co piece (State.pieces st))

        let removeFromHand (_,((id: uint32),_)) st =
            st |> State.withHand (MultiSet.removeSingle id (State.hand st))

        let emptyHand st =
            st |> State.withHand MultiSet.empty

        let addToHand (id,n) st =
            st |> State.withHand (MultiSet.add id n (State.hand st))


        let explodeTile id = Map.find id pieces |> Set.toList |> List.map (fun t' -> (id, t'))
        
        // We use this to turn the hands into all the possible letters to choose from in the hand
        // This is needed to handle blank pieces
        let explodeHand (hand: msetHand): explHand =
            let f mset id n =
                let explodedTile = explodeTile id
                List.fold (fun s l -> MultiSet.add l n s) mset explodedTile
            MultiSet.fold f MultiSet.empty hand

        let removeFromExplodedHand ((id,(c,v)) as l: letter) (hand: explHand) =
            // We want to remove all parts of the explodede hand which partially represent the letter.
            // To do this, we look up the full tile for the letter using the id, and remove one
            // instance of every form of it.
            let fullTile = explodeTile id
            List.fold (fun mset p -> MultiSet.removeSingle p mset) hand fullTile


        let doMove st =

            /// Returns a sequence of valid words found in `dict` using the letters from `hand`
            let validWordSeq hand dict n : letter list seq =
                let rec auxSeq (p: letter) (w: letter list, d: Dictionary.Dict, h: explHand): letter list seq =
                    let (_,(c,_)) = p // Extract the char from the letter
                    let nextDict = Dictionary.step c d
                    match nextDict with
                    | None -> Seq.empty // We did not find a word on this path
                    | Some (b', d') ->
                        let currentWord = w @ [p] // Current word + the character we searched for
                        debugPrint (sprintf "        └─> word: \"%A\"\n            └─> hand: %A\n" (List.map (fun (id,(c,_)) -> (id,c)) currentWord) (MultiSet.map (fun (id,(c,_)) -> (id,c)) h))
                        seq {
                            for p in MultiSet.toList h do // toList is not ideal, but our multiset does not have a way to be iterated as a sequence :(
                                if b' then yield currentWord // Yield if this is a word
                                yield! auxSeq p (currentWord, d', removeFromExplodedHand p h) // Continue search
                        }

                seq {
                    for p in MultiSet.toList hand do
                        yield! auxSeq p ([], dict, removeFromExplodedHand p hand) |> Seq.truncate n
                }

            /// Returns the first valid continuation from `start` using the letters from `hand`
            /// (continuations do *not* include the first letter `start`)
            let finishWordSeq (start: char) hand n : letter list seq =
                let initialDict = Dictionary.step start (State.dict st)
                match initialDict with
                    | None -> Seq.empty
                    | Some (_,dict) -> (validWordSeq hand dict) n
            
            let isFirstMove (p: Map<coord, (char * int)>): bool = Map.isEmpty p

            let findStartPositions (st: State.state): ((coord * char) list * (coord * char) list) =
                let f k v (accH,accV) =
                    let hasNeighborRight = Map.containsKey (Coord.mkCoordinate ((Coord.getX k) + 1) (Coord.getY k)) (State.pieces st)
                    let hasNeighborDown  = Map.containsKey (Coord.mkCoordinate (Coord.getX k) ((Coord.getY k) + 1)) (State.pieces st)
                    ((
                        match hasNeighborRight with
                        | false -> (k,v |> fst)::accH
                        | true  ->               accH
                    ),(
                        match hasNeighborDown with
                        | false -> (k,v |> fst)::accV
                        | true  ->               accV
                    ))
                Map.foldBack f (State.pieces st) ([],[])

            let canDoMove (p: Map<coord, (char * int)>) (move: (coord * (uint32 * (char * int))) list) =
                
                // Does it intersect with existing pieces?
                let checkOverlap m =
                    let coords = List.map (fun (xy,_) -> xy) m
                    let f acc xy =
                        match acc with
                        | false -> false
                        | true  -> not (Map.containsKey xy p)
                    match List.fold f true coords with
                    | true  -> Some m
                    | false -> None
                
                // Does it create invalid words in with neighbor pieces?
                
                let performMove ps ms =
                    let placePieces p' (co: coord, (_,(piece: char * int))) =
                        Map.add co piece p'
                    List.fold (fun p' m -> placePieces p' m) ps ms
                
                let checkWords travelBack travelForward p' m =
                    let rec furthestLeftCoord pos =
                        let prevNeighborCoord = travelBack pos
                        match Map.containsKey prevNeighborCoord p' with
                        | true  -> furthestLeftCoord prevNeighborCoord
                        | false -> pos
                    let rec word (acc: string, pos) =
                        match Map.tryFind pos p' with
                        | None       -> acc
                        | Some (c:char,_) -> word (acc + string c, travelForward pos)
                    
                    let f acc (xy:coord,_) = // Checks if a valid word is created with by the piece at `xy`
                        match acc with
                        | false -> false
                        | true  -> 
                            let w = word ("", furthestLeftCoord xy)
                            match String.length w with
                            | 1 -> true // Don't check single letters
                            | _ ->
                                debugPrint (sprintf "            └─> check word: %A\n" w)
                                Dictionary.lookup (w) (State.dict st)
                    match List.fold f true m with
                        | false -> None
                        | true  -> Some m

                debugPrint (sprintf "            └─> overlap: %A\n" (Option.isNone (checkOverlap move)))
                match checkOverlap move with
                | None   -> None // Invalid, evaluate none
                | Some m ->      // No overlap, continue
                    let p' = performMove p m
                    let checkWordsHorizontal =
                        checkWords
                            (fun xy -> (Coord.mkCoordinate ((Coord.getX xy) - 1) (Coord.getY xy)))
                            (fun xy -> (Coord.mkCoordinate ((Coord.getX xy) + 1) (Coord.getY xy)))
                            p'
                    let checkWordsVertical =
                        checkWords
                            (fun xy -> (Coord.mkCoordinate (Coord.getX xy) ((Coord.getY xy) - 1)))
                            (fun xy -> (Coord.mkCoordinate (Coord.getX xy) ((Coord.getY xy) + 1)))
                            p'

                    match checkWordsHorizontal m with
                    | None   -> None
                    | Some m -> checkWordsVertical m


            let wordToMoveHorizontal offset (start: coord) word =
                let f i (p: letter) =
                    ((Coord.mkCoordinate ((Coord.getX start) + offset + i) (Coord.getY start)), p)
                List.mapi f word

            let wordToMoveVertical offset (start: coord) word =
                let f i (p: letter) =
                    ((Coord.mkCoordinate (Coord.getX start) ((Coord.getY start) + offset + i)), p)
                List.mapi f word




            // Main part            
            //System.Console.ReadLine() |> ignore

            //Print.printHand pieces (State.hand st)


            let move: (coord * letter) list option =        
                match isFirstMove (State.pieces st) with
                | true  -> // This is the first move
                    // Find valid word only from hand
                    let validWords = validWordSeq (State.hand st |> explodeHand) (State.dict st) 100
                    match Seq.isEmpty validWords with
                    | true  -> None // Do nothing (change pieces)
                    | false ->
                        let w = Seq.head validWords 
                        Some (wordToMoveHorizontal 0 (State.board st).center w) // Play word horizontally
                    
                | false -> // This is *not* the first move
                    debugPrint "=======> Not first move\n"
                    // Find positions we can start a word from
                    let (hStartPositions, vStartPositions) = findStartPositions st
                    debugPrint (sprintf "└─> hStart: %A\n    vStart: %A\n" hStartPositions vStartPositions)
                    
                    let f wordToMove acc (pos, c:char) =
                        match acc with
                        | Some m -> Some m
                        | None   ->
                            debugPrint (sprintf "    └─> pos: %A\n" pos)
                            let maxWordsPerPos = 5 // We limit how many words we try to increase speed, but we could
                                                   // check all options by changing this to infinity
                            let wordSeq = finishWordSeq c (State.hand st |> explodeHand) maxWordsPerPos
                            let moveSeq = Seq.map (fun w -> wordToMove pos w) wordSeq // Convert words to moves starting at `pos`
                            //let preferredWord = Seq.tryFind (fun m -> Option.isSome (canDoMove (State.pieces st) m)) moveSeq
                            let canDoPreferredMove m =
                                match m with
                                | m when List.length m < PREFERRED_WORD_LENGTH -> false
                                | m -> Option.isSome (canDoMove(State.pieces st) m)
                            let preferredWord = Seq.tryFind canDoPreferredMove moveSeq
                            match preferredWord with
                            | Some w -> Some w
                            | None   -> Seq.tryFind (fun m -> Option.isSome (canDoMove (State.pieces st) m)) moveSeq
                            (*
                                Doing the preferred move this way is very slow. It would be better to make some custom
                                implementation of tryFind that also returns the first word we found, but this is a lot
                                simpler.
                            *)
                    
                    match List.fold (f (wordToMoveHorizontal 1)) None hStartPositions with
                    | Some m -> Some m
                    | None   -> List.fold (f (wordToMoveVertical 1)) None vStartPositions
                    


            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            match move with
            | None   -> send cstream (SMChange (st |> State.hand |> MultiSet.toList))
            | Some m -> send cstream (SMPlay m)
            
            move



        let rec aux (st : State.state) =

            let hasTurn =
                (State.turn st) = (State.playerNumber st)

            let move = match hasTurn with
                       | false -> None
                       | true  -> Some (doMove st)
            move |> ignore

            // Recieve message from server and bind it to `msg`.
            debugPrint (sprintf "====> Listening for msg, turn is %d\n" (State.turn st))
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) msg) // keep the debug lines. They are useful.


            // Handle the response from the server.
            match msg with
            | RCM (CMPlaySuccess(ms, _, newPieces)) ->
                debugPrint "====> PLAY SUCCESS\n"
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                // Place pieces on board
                // Remove pieces from hand
                // Add new pieces to hand
                let performMove st =
                    List.fold (fun s m -> s
                                       |> placePieces m    // place pieces
                                       |> removeFromHand m // remove pieces from hand
                    ) st ms
                let addNewTilesToHand st =
                    List.fold (fun s np -> s
                                        |> addToHand np // add piece(s) to hand
                    ) st newPieces
                let updateTurn st = st
                                    |> nextTurn (State.playerNumber st) // We know we made this move
                // Combine to update state
                let st' = st |> performMove |> addNewTilesToHand |> updateTurn
                aux st'


            | RCM (CMPlayed (pid, ms, _)) ->
                debugPrint "====> PLAYED\n"
                debugPrint (sprintf "play by: %A\n" pid)
                (* Successful play by other player. Update your state *)
                // Place tiles on board
                let performMove st =
                    List.fold (fun s m -> s
                                       |> placePieces m
                    ) st ms
                let updateTurn st = st
                                    |> nextTurn pid
                let st' = st |> performMove |> updateTurn
                aux st'


            | RCM (CMPlayFailed (pid, _)) ->
                debugPrint "====> PLAY FAILED\n"
                (* Failed play. Update your state *)
                // Only update turn
                let updateTurn st = st
                                    |> nextTurn pid
                let st' = st |> updateTurn
                aux st'
            

            | RCM (CMChangeSuccess (newPieces)) ->
                debugPrint "====> CHANGE SUCCESS\n"
                (* Successful change by you. *)
                // Remove pieces from hand
                // Add new pieces to hand
                let addNewTilesToHand st =
                    List.fold (fun s np -> s
                                        |> addToHand np // add piece(s) to hand
                    ) st newPieces
                let updateTurn st = st
                                    |> nextTurn (State.playerNumber st) // We know we made this move
                // Combine to update state
                let st' = st |> emptyHand |> addNewTilesToHand |> updateTurn
                aux st'


            | RCM (CMChange (pid, _)) ->
                debugPrint "====> CHANGE\n"
                (* Successgul chenge by other player. *)
                // Only update turn
                let updateTurn st = st
                                    |> nextTurn pid
                let st' = st |> updateTurn
                aux st'
            

            | RCM (CMPassed (pid)) ->
                debugPrint "====> PASSED\n"
                (* Other player passed. *)
                // Only update turn
                let updateTurn st = st
                                    |> nextTurn pid
                let st' = st |> updateTurn
                aux st'
            

            | RCM (CMForfeit (pid)) ->
                debugPrint "====> FORFEIT"
                debugPrint(sprintf "forfeit by: %A\n" pid)
                (* Other player forfeited *)
                // Update dropped players
                // Update turn.
                debugPrint(sprintf "players: %A\n" (State.totalPlayers st))
                let updateDroppedPlayers st =
                    let dp = State.droppedPlayers st
                    let newDp = Set.add pid dp
                    st |> State.withDroppedPlayers newDp
                let updateTurn st = st
                                    |> nextTurn (pid)
                let st' = st |> updateDroppedPlayers |> updateTurn
                debugPrint(sprintf "players: %A\n" (State.totalPlayers st'))
                aux st'
            

            | RCM (CMTimeout (pid)) ->
                debugPrint "====> TIMEOUT\n"
                (* Other player passed. *)
                // Only update turn
                let updateTurn st = st
                                    |> nextTurn pid
                let st' = st |> updateTurn
                aux st'


            | RCM (CMGameOver _) -> ()
            | RGPE err -> 
                debugPrint "====> GAMEPLAY ERROR"
                let f e = 
                    (*
                        We are not told in startGame how many pieces are in the "bag" from the start, so we cannot
                        track the amount of pieces left. Instead, we use the NotEnoughPieces error to handle situations
                        where we cannot get the 7 pieces we ask for.
                        (We could assume there are always 100 pieces, but we don't want to rely on such an assumption)
                    *)
                    match e with
                    | GPENotEnoughPieces (_, n) ->
                        send cstream (SMChange (st |> State.hand |> MultiSet.toList |> List.take (int n)))
                    | _ ->
                        printfn "Gameplay Error:\n%A" err;
                List.iter f err
                aux st


        aux st

    let startGame 
            (boardP : boardProg)                // Scrabble board
            (dictf : bool -> Dictionary.Dict)   // Dictionary
            (numPlayers : uint32)               // Number of players
            (playerNumber : uint32)             // Our player number
            (playerTurn  : uint32)              // Starting player number
            (hand : (uint32 * uint32) list)     // Starting hand
            (tiles : Map<uint32, tile>)         // Tile lookup table
            (timeout : uint32 option)           // Timeout in milliseconds
            (cstream : Stream) =                // Server communication channel
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty numPlayers playerTurn Set.empty)
        