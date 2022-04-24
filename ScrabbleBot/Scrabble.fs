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

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        pieces        : Map<coord, (char * int)>
        totalPlayers  : uint32
        turn          : uint32
    }

    let mkState b d pn h p tp t =
        {board = b; dict = d;  playerNumber = pn; hand = h; pieces = p; totalPlayers = tp; turn = t;}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let pieces st        = st.pieces
    let totalPlayers st  = st.totalPlayers
    let turn st          = st.turn

    let withBoard b st         = {st with board = b}
    let withDict d st          = {st with dict = d}
    let withPlayerNumber pn st = {st with playerNumber = pn}
    let withHand h st          = {st with hand = h}
    let withPieces p st        = {st with pieces = p}
    let withTotalPlayers tp st = {st with totalPlayers = tp}
    let withTurn t st          = {st with turn = t}

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let nextTurn t st =
            st |> State.withTurn ((t % (State.totalPlayers st)) + 1u) // IDs start from 1

        let placePieces (co: coord, (_,(piece: char * int))) st =
            st |> State.withPieces (Map.add co piece (State.pieces st))

        let removeFromHand (_,((id: uint32),_)) st =
            st |> State.withHand (MultiSet.removeSingle id (State.hand st))

        let addToHand (id,n) st =
            st |> State.withHand (MultiSet.add id n (State.hand st))


        let doMove hasTurn st =
            match hasTurn with
            | false -> None
            | true  ->
                Print.printHand pieces (State.hand st)

                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                // This is the part where the move is defined:
                (*
                    This part will be replaced with our move-finding algorithm.
                    The result will be stored in the `move` variable.
                *)
                let input =  System.Console.ReadLine() // Human input
                let move = RegEx.parseMove input // Input parsed into an actual move

                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                // Send move to server. This could also be other types of move such as SMChange.
                send cstream (SMPlay move)

                Some move


        let rec aux (st : State.state) =

            let hasTurn =
                (State.turn st) = (State.playerNumber st)

            let move = doMove hasTurn st
            move |> ignore

            // Recieve message from server and bind it to `msg`.
            debugPrint (sprintf "====> Listening for msg, turn is %d\n" (State.turn st))
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) msg) // keep the debug lines. They are useful.


            // Handle the response from the server.
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
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


            | RCM (CMPlayed (pid, ms, points)) ->
                debugPrint "====> PLAYED\n"
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


            | RCM (CMPlayFailed (pid, ms)) ->
                debugPrint "====> PLAY FAILED\n"
                (* Failed play. Update your state *)
                // Only update turn
                let updateTurn st = st
                                    |> nextTurn pid
                let st' = st |> updateTurn
                aux st'


            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty numPlayers playerTurn)
        