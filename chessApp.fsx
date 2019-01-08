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
  let _player = 0
  let _board = Chess.Board () 

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
    fun () -> 
      _player := (!_player + 1) % 2
      _players.[!_player]
  member self.isGameOver (b:Board) : bool =
    // TODO Check om kongen har available moves
    false 
    
  member self.isValidMove (move:Move) : bool = 
    let _isAvailableMove (_piece:chessPiece) (_move:Move) = 
      _piece.candiateRelativeMoves
      |> List.map (fun el -> el.Head) // Stored as a list list for some reaseon
      |> _board.relativeToAbsolute (fst move)
      |> fun a -> 
        match List.tryFind (fun mv -> mv = (snd move)) a with
          | None -> false 
          | Some _ -> true

    let piece : chessPiece option = 
      _board.[(move |> fst |> fst), (move |> snd |> snd)]
    if piece.IsNone then 
      false 
    elif _isAvailableMove piece.Value move |> not then 
      false 
    else
      true
  member self.run (curPlayer:Player) (board:Board) = 
    match self.isGameOver board with 
    | true -> 0
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
let board = Chess.Board () // Create a board
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
Array.iter (printPiece board) pieces
