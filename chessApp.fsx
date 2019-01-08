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
    let format = RegularExpressions.Regex "[a-h][1-8][a-h][1-8]|quit"
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

let isNone (obj:'a option) = match obj with |Some x -> false | None -> true

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
    let format = RegularExpressions.Regex "([a-h])([1-8])([a-h])([1-8])"
    let fileString = "abcdefgh"
    let rankString = "12345678"
    let matches = format.Match moveString
    // printfn "%A" matches.Groups
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


  member self.switchPlayer () = 
    _playerIndex := (!_playerIndex + 1) % 2
  member self.isGameOver (board:Board) (P:Player) : bool =
    let mutable isOver = false
    for i=0 to 7 do
      for j= 0 to 7 do
          match board.Item (i,j) with
          | Some x when (x :? king && P.color = x.color) ->
              match x.position with
              | Some i ->
                let position_lst = fst (board.getVacantNNeighbours x)
                let mutable move_lst = []
                
                for entry in position_lst do
                  move_lst <- (i,entry)::move_lst
               
                match (List.filter (fun elem -> self.isInvalidMove elem P |> isNone) move_lst) with
                |[] -> isOver <- true
                |_ -> ()
              |_ -> ()
          |_ -> ()
    // printfn "%A" (isOver)
    isOver

  member self.getAbsoluteMoves (origo:Position) (p:chessPiece) = 
    p.candiateRelativeMoves |> List.concat
    |> List.map (self.relativeToAbsolutePos origo)
    
  member self.relativeToAbsolutePos (origo:Position) (rPos:Position) = 
    ( (fst origo) + (fst rPos), ( (snd origo) + (snd rPos) )  )

    // This is string option so we can return with a status code
  member self.isInvalidMove (move:Move) (p:Player) : string option = 
    let _isWithinBounds (target:Position) = 
      ( (fst target) < 8 && (fst target) >= 0) && ( (snd target ) < 8 && (snd target) >= 0)
    
    let _isAvailableMove (_piece:chessPiece) (_move:Move) = 
      // let relativeMoves =_piece.candiateRelativeMoves  
      // let expandedMoves = List.concat relativeMoves
      // let absoluteMoves = List.map (fun x -> self.relativeToAbsolutePos (fst _move) x) expandedMoves
      let absoluteMoves = self.getAbsoluteMoves (fst _move) _piece
      match List.tryFind (fun x -> x = (snd _move) ) absoluteMoves with 
      | Some x -> true 
      | None -> false

    let _isKingAndThreatened (_piece:chessPiece) (_move:Move) =
      if _piece.nameOfType = "king" |> not then false 
      else 
        let mutable isThreatened = false
        // Get a list of chess pieces
        let mutable pieceList = []
        for i = 0 to 7 do 
          for j = 0 to 7 do 
            match _board.[i,j] with 
            | None -> ()
            | Some piece -> pieceList <- (piece, i, j) :: pieceList
        
        // Filter out our own pieces
        let filteredPieceList = List.filter (fun (piece:chessPiece,_,_) -> piece.color <> p.color ) pieceList 
        
        // loop through opponent pieces
        for (p,i,j) in filteredPieceList do 
          // If it has an absolute move on our _move then isThreatened = true
          let availableMoves = self.getAbsoluteMoves (i,j) p 
          for m in availableMoves do 
            if m = (snd _move) then
              // printfn "Move %A is threatened by %A on space %A" _move p.color (i,j)
              isThreatened <- true
        isThreatened
    let piece : chessPiece option = 
      _board.[(move |> fst |> fst), (move |> fst |> snd)]
    if piece.IsNone then
      Some (sprintf "Piece is none. %A" ((move |> fst |> fst), (move |> fst |> snd)))
      
    elif piece.Value.color <> p.color then 
      sprintf "You can't move another players pawn" |> Some
      
    elif _isWithinBounds (snd move) |> not then 
      sprintf "Is not within bounds" |> Some
    elif _isAvailableMove piece.Value move |> not then 
      sprintf "Piece doesn't have this move as available. %A" ((move |> snd |> fst), (move |> snd |> snd)) |> Some
      
    elif _isKingAndThreatened piece.Value move  then
      sprintf "King tries to move on threaten"|> Some
    else
      None

  member self.board 
    with get () = _board
  member self.run (curPlayer:Player) (board:Board) =
    printfn "%A" board 
    match self.isGameOver board curPlayer with 
    | true -> self.switchPlayer () |> ignore; !_playerIndex
    | false -> 
    let move = curPlayer.nextMove ()
    if move = "quit" then
      -1
    else 
      match self.parseMoveString move with 
      | x when self.isInvalidMove x curPlayer |> isNone -> 
        x
        |> (fun (target,source) -> printfn "Move is valid, target %A src %A" target source ; target, source)
        ||> _board.move 
        |> ignore
        self.switchPlayer ()
        self.run _players.[!_playerIndex]  board
      | x -> 
        printfn "Invalid move: %s" (self.isInvalidMove x curPlayer).Value
        self.run curPlayer board

let p1 = Human (White) :> Player
let p2 = Human (Black) :> Player

let game = Game (p1,p2)
do game.newGame () |> printfn "%d"
