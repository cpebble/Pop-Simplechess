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
type Player() = 
  abstract member nextMove : unit  -> string

type Human() = 
  inherit Player()
  member self.isValidMoveString (m: string) = 
    let format = RegularExpressions.Regex "[a-f][1-8][a-f][1-8]|quit"
    format.IsMatch m
  override this.nextMove () =
    // TODO Læs fra en terminal
    let move = System.Console.ReadLine () 
    if this.isValidMoveString move then 
      move 
    else 
      this.nextMove ()


// Game class
type Game(p1:Player, p2:Player) =
  let _players = [p1;p2]
  let _playerIndex = ref 0
  let mutable _pieces = [
    king(White) :> chessPiece
    king(Black) :> chessPiece
    rook(White) :> chessPiece
    rook(White) :> chessPiece
    rook(Black) :> chessPiece
    rook(Black) :> chessPiece

  ]
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
  
  

  member this.parseMoveString (moveString:string) : Move = 
    // Tager input a4a6 -> 0,3 , 0,5
    let format = RegularExpressions.Regex "([a-f])([1-8])([a-f])([1-8])"
    let fileString = "abcdefgh"
    let rankString = "12345678"
    let matches = format.Match moveString
    let srcFile =(
       matches.Captures.[0].Value 
      |> fileString.IndexOf 
    )
    let srcRank = (
      matches.Captures.[1].Value
      |> rankString.IndexOf
    )
    let targetFile =(
       matches.Captures.[2].Value
      |> fileString.IndexOf
    )
    let targetRank =(
       matches.Captures.[3].Value
      |> rankString.IndexOf
    )
    (srcFile, srcRank), (targetFile, targetRank)


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
            if m.Head = move then
              is_threatened <- true
        is_threatened 
        // Check if move is threatened
    let piece : chessPiece option = 
      _board.[(move |> fst |> fst), (move |> snd |> snd)]
    if piece.IsNone then 
      false 
    elif _isAvailableMove piece.Value move |> not then 
      false 
    elif _isKingAndThreatened piece.Value move |> not then
      false
    else
      true

  member self.run (curPlayer:Player) (board:Board) = 
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
        |> (fun (target,source) -> target, source)
        ||> _board.move 
        |> ignore
        self.run (self.nextPlayer () ) board
      | _ -> self.run curPlayer board



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
