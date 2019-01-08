#load "chess.fs"
#load "pieces.fs"
open Chess
open Pieces
open System.Text
/// Print various information about a piece
let printPiece (board : Board) (p : chessPiece) : unit =
  printfn "%A: %A %A" p p.position (p.availableMoves board)

type Move = ((int * int) * (int * int))

[<AbstractClass>]
type Player(color) = 
  abstract member nextMove : unit  -> string
  abstract member color : Color 

type Human(color) = 
  inherit Player()
  member self.isValidMoveString (m: string) = 
    let format = RegularExpressions.Regex "[a-f][1-8][a-f][1-8]|quit"
    format.IsMatch m
  override this.nextMove () =
    // TODO Læs fra en terminal 
    printf "[%s]Input move: " (match this.color with | White -> "white" | Black -> "black")
    let move = System.Console.ReadLine () 
    if this.isValidMoveString move then 
      move 
    else 
      this.nextMove ()
  override this.color = color


// Game class
type Game(p1:Player, p2:Player) =
  let _players = [p1;p2]
  let _playerIndex = ref 0
  let mutable _pieces = [  ]

  let pieceIsDead (piece:chessPiece) (pos:Position) = 
    let rec remove i l =
      match i, l with
      | 0, x::xs -> xs
      | i, x::xs -> x::remove (i - 1) xs
      | i, [] -> failwith "index out of range"
    let pieceIndex = List.findIndex (fun p -> p = piece) _pieces
    _pieces <- remove pieceIndex _pieces
    ()
  let _board = Chess.Board (pieceIsDead) 
  
  member self.newGame () =
    let _startPositions = [
      (king(White) :> chessPiece, 4,0 );
      (king(Black):> chessPiece, 5,7 );
      (rook(White):> chessPiece, 0,0 );
      (rook(White):> chessPiece, 7,0 );
      (rook(Black):> chessPiece, 7,7 );
      (rook(Black):> chessPiece, 0,7 );
    ]
    for (piece, rank, file) in _startPositions do 
      _pieces <- piece :: _pieces 
      _board.[file,rank] <- Some piece
    done
    self.run p1 self.board


  member this.parseMoveString (moveString:string) : Move = 
    // Tager input a4a6 -> 0,3 , 0,5
    let format = RegularExpressions.Regex "([a-f])([1-8])([a-f])([1-8])"
    let fileString = "abcdefgh"
    let rankString = "12345678"
    let matches = format.Match moveString
    printfn "%A" matches.Groups
    let srcFile =(
      matches.Groups.[1].Value 
      |> fileString.IndexOf 
    )
    let srcRank = (
      matches.Groups.[2].Value
      |> rankString.IndexOf
    )
    let targetFile =(
      matches.Groups.[3].Value
      |> fileString.IndexOf
    )
    let targetRank =(
      matches.Groups.[4].Value
      |> rankString.IndexOf
    )
    (srcRank, srcFile), (targetRank, targetFile)


  member self.nextPlayer = 
    let _player = ref 0
    _playerIndex = _player |> ignore
    fun () -> 
      _player := (!_player + 1) % 2
      _players.[!_player]
  member self.isGameOver (board:Board) : bool =
    // TODO Check om kongen har available moves
    let mutable king_count = 0
    for i=0 to 7 do
        for j= 0 to 7 do
            match board.Item (i,j) with
            | Some x ->
                if x :? king then king_count <- king_count + 1                   
            | _ -> ()
    if king_count < 2 then
        true
    else
        false
    
  member self.isValidMove (move:Move) : bool = 
    let _isAvailableMove (_piece:chessPiece) (_move:Move) = 
      _piece.candiateRelativeMoves
      |> List.map (fun el -> el.Head) // Stored as a list list for some reaseon
      |> _board.relativeToAbsolutePos (fst _move)
      |> fun a -> 
        match List.tryFind (fun mv -> mv = (snd _move)) a with
          | None -> false 
          | Some _ -> true
    let _isKingAndThreatened (_piece:chessPiece) (_move:Move) =
      if _piece.nameOfType = "king" |> not then false 
      else 
        let move = snd _move 
        let allPieces = _pieces //indeholder alle brikker på boardet 
        let mutable dangerPos = []
        let mutable is_threatened = false
        for p in allPieces do
          for m in p.candiateRelativeMoves do
            if m.Head = move && p <> _piece then
              is_threatened <- true
        is_threatened 
        // Check if move is threatened
    let piece : chessPiece option = 
      _board.[(move |> fst |> fst), (move |> snd |> snd)]
    if piece.IsNone then
      printfn "Piece is none. %A" ((move |> fst |> fst), (move |> snd |> snd))
      false 
    elif _isAvailableMove piece.Value move |> not then 
      printfn "Piece doesn't have this move as available. %A\nAvailable position:" ((move |> snd |> fst), (move |> snd |> snd)) 
      for i in piece.Value.availableMoves do 
         printf "%A\t" i 
      false 
    elif _isKingAndThreatened piece.Value move |> not then
      printfn "King tries to move on threaten"
      false
    else
      true

  member self.board 
    with get () = _board
  member self.run (curPlayer:Player) (board:Board) =
    printfn "%A" board 
    match self.isGameOver board with 
    | true -> self.nextPlayer (); !_playerIndex
    | false -> 
    let move = curPlayer.nextMove ()
    if move = "quit" then
      -1
    else 
      match self.parseMoveString move with 
      | x when self.isValidMove x -> 
        x
        |> (fun (target,source) -> printf "Move is walid, target %A src %A" target source ; target, source)
        ||> _board.move 
        |> ignore
        self.run (self.nextPlayer () ) board
      | _ -> self.run curPlayer board



let p1 = Human (White) :> Player
let p2 = Human (Black) :> Player

let game = Game (p1,p2)
do game.newGame () |> printfn "%d"
// Create a game
(*let board = Chess.Board () // Create a board
// Pieces are kept in an array for easy testing
let pieces = [|
  king (White) :> chessPiece;
  rook (White) :> chessPiece;
  king (Black) :> chessPiece |]
// Place pieces on the board
board.[0,0] <- Some pieces.[0]
board.[1,1] <- Some pieces.[1]
board.[4,1] <- Some pieces.[2]
printfn "%A" board
Array.iter (printPiece board) pieces

// Make moves
board.move (1,1) (3,1) // Moves a piece from (1,1) to (3,1)
printfn "%A" board
Array.iter (printPiece board) pieces*)
