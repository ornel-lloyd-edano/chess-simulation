package chess.domain

import chess.domain.Board._
import chess.domain.pieces.ChessPiece.{ChessPieceType, Color}
import chess.domain.pieces._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class Board(state: State) {

  //a snapshot of the current state before any King makes a move
  //if the King is check mated after its own move, current state is rolled back to previous state
  private var previousState = new State(state.state)

  def get(tile: Tile):Option[ChessPiece] = state.tiles.get(tile)

  def get(chessPiece: ChessPiece):Option[Tile] = state.chessPieces.get(chessPiece)

  def set(chessPiece: ChessPiece, destination: Tile): Try[ChessPiece] = {
    chessPiece.getAllMoves(state).find(_ == destination) match {
      case None=>
        Failure(new Exception(s"Tile $destination is illegal move for $chessPiece"))
      case Some(validDestTile)=>
        if (!chessPiece.isBlocked(validDestTile)(state)) {
          state.set(chessPiece, validDestTile)
        } else {
          val message = if (chessPiece.`type` == ChessPieceType.King) {
            s"Tile $destination is check for $chessPiece"
          } else {
            s"Tile $destination is blocked and unreachable for $chessPiece"
          }
          Failure(new Exception(message))
        }
    }
  }
}

object Board {
  class State(val state: Map[Tile, ChessPiece]) extends ReadableBoardState {

    private val mutableState = mutable.Map[Tile, ChessPiece]()
    initState(state)

    private def initState(src: Map[Tile, ChessPiece]): Unit = {
      mutableState.clear()
      src.foreach { case (tile, chessPiece)=> mutableState.put(tile, chessPiece) }
    }

    /**
     * Gets all the tiles occupied by all uncaptured chess pieces
     * @return a table of Tile locations and the corresponding chess piece on top of it
     */
    override def tiles: Map[Tile, ChessPiece] = mutableState.toMap

    /**
     * Gets all the chess pieces currently occupying the tiles
     * @return a table of Chess pieces and the corresponding tile locations each occupy
     */
    override def chessPieces: Map[ChessPiece, Tile] = mutableState.map(_.swap).toMap

    /**
     * Sets a chess piece to a desired location, regardless if it is a valid move or not, while updating the board state
     * @param chessPiece indicated by the id, type, color and current tile location
     * @param destination the new tile location to put the chosen chess piece
     * @return a copy of the chess piece with updated current tile location if success else an exception
     */
    def set(chessPiece: ChessPiece, destination: Tile): Try[ChessPiece] = {
      chessPieces.get(chessPiece).map { tileToRemove=>
        val relocatedChessPiece = chessPiece.clone(destination)
        val newState = (tiles - tileToRemove) + (destination -> relocatedChessPiece)
        initState(newState)
        Success(relocatedChessPiece)
      }.getOrElse(Failure(new Exception(s"$chessPiece not found")))
    }

    /**
     * Clears a tile from chess piece occupancy, a.k.a. removes a chess piece from the board
     * @param tile any tile from the chess board from a1 to h8
     * @return nothing if successfully removed or was already removed, exception if attempting to remove a King
     */
    def removeAt(tile: Tile): Try[Unit] = {
      tiles.get(tile) match {
        case Some(King(_, _))=>
          Failure(new Exception(s"Invalid state: King at tile $tile cannot be removed"))
        case _=>
          initState(tiles - tile)
          Success(())
      }
    }

    /**
     * Checks if the chess pieces in the game are in the correct count based on color and type
     * @return the state itself if valid or a list of exceptions if invalid state
     */
    def validate: Either[Seq[Exception], State] = {
      def notMoreThanN(nCount: Int, chessPieceType: ChessPiece.ChessPieceType, color: ChessPiece.Color): Boolean = {
        tiles.count {
          case (_, chessPiece)=> chessPiece.`type` == chessPieceType && chessPiece.color == color
        } <= nCount
      }
      def alwaysExactlyOne(chessPieceType: ChessPiece.ChessPieceType, color: ChessPiece.Color): Boolean = {
        tiles.find {
          case (_, chessPiece)=> chessPiece.`type` == chessPieceType && chessPiece.color == color
        }.isDefined
      }

      val whiteKingExists = if (alwaysExactlyOne(ChessPiece.ChessPieceType.King, ChessPiece.Color.White))
        Right(this) else Left(new Exception("Invalid state: No White King"))

      val blackKingExists = if (alwaysExactlyOne(ChessPiece.ChessPieceType.King, ChessPiece.Color.Black))
        Right(this) else Left(new Exception("Invalid state: No Black King"))

      val whiteQueenZeroOrOne = if (notMoreThanN(1, ChessPiece.ChessPieceType.Queen, ChessPiece.Color.White))
        Right(this) else Left(new Exception("Invalid state: More than 1 White Queen"))

      val blackQueenZeroOrOne = if (notMoreThanN(1, ChessPiece.ChessPieceType.Queen, ChessPiece.Color.Black))
        Right(this) else Left(new Exception("Invalid state: More than 1 Black Queen"))

      val whiteBishopNotMoreThanTwo = if (notMoreThanN(2, ChessPiece.ChessPieceType.Bishop, ChessPiece.Color.White))
        Right(this) else Left(new Exception("Invalid state: More than 2 White Bishops"))

      val blackBishopNotMoreThanTwo = if (notMoreThanN(2, ChessPiece.ChessPieceType.Bishop, ChessPiece.Color.Black))
        Right(this) else Left(new Exception("Invalid state: More than 2 Black Bishops"))

      val whiteKnightNotMoreThanTwo = if (notMoreThanN(2, ChessPiece.ChessPieceType.Knight, ChessPiece.Color.White))
        Right(this) else Left(new Exception("Invalid state: More than 2 White Knights"))

      val blackKnightNotMoreThanTwo = if (notMoreThanN(2, ChessPiece.ChessPieceType.Knight, ChessPiece.Color.Black))
        Right(this) else Left(new Exception("Invalid state: More than 2 Black Knights"))

      val whiteRookNotMoreThanTwo = if (notMoreThanN(2, ChessPiece.ChessPieceType.Rook, ChessPiece.Color.White))
        Right(this) else Left(new Exception("Invalid state: More than 2 White Rooks"))

      val blackRookNotMoreThanTwo = if (notMoreThanN(2, ChessPiece.ChessPieceType.Rook, ChessPiece.Color.Black))
        Right(this) else Left(new Exception("Invalid state: More than 2 Black Rooks"))

      val whitePawnNotMoreThanEight = if (notMoreThanN(8, ChessPiece.ChessPieceType.Pawn, ChessPiece.Color.White))
        Right(this) else Left(new Exception("Invalid state: More than 8 White Pawns"))

      val blackPawnNotMoreThanTwo = if (notMoreThanN(8, ChessPiece.ChessPieceType.Pawn, ChessPiece.Color.Black))
        Right(this) else Left(new Exception("Invalid state: More than 8 Black Pawns"))

      val invalidStates = Seq(whiteKingExists, blackKingExists, whiteQueenZeroOrOne, blackQueenZeroOrOne,
        whiteBishopNotMoreThanTwo, blackBishopNotMoreThanTwo, whiteKnightNotMoreThanTwo, blackKnightNotMoreThanTwo,
        whiteRookNotMoreThanTwo, blackRookNotMoreThanTwo, whitePawnNotMoreThanEight, blackPawnNotMoreThanTwo)
        .filter(_.isLeft)

      if (invalidStates.isEmpty) Right(this) else Left(invalidStates.collect{ case Left(exception)=> exception })
    }
  }

  object State {
    private val tiles = Map[Tile, ChessPiece](
      Tile("a1") -> new Rook(1, Color.White, Tile("a1")),
      Tile("b1") -> new Knight(1, Color.White, Tile("b1")),
      Tile("c1") -> new Bishop(1, Color.White, Tile("c1")),
      Tile("d1") -> new Queen( Color.White, Tile("d1")),
      Tile("e1") -> new King( Color.White, Tile("e1")),
      Tile("f1") -> new Bishop(2, Color.White, Tile("f1")),
      Tile("g1") -> new Knight(2, Color.White, Tile("g1")),
      Tile("h1") -> new Rook(2, Color.White, Tile("h1")),
      Tile("a2") -> new Pawn(1, Color.White, Tile("a2")),
      Tile("b2") -> new Pawn(2, Color.White, Tile("b2")),
      Tile("c2") -> new Pawn(3, Color.White, Tile("c2")),
      Tile("d2") -> new Pawn(4, Color.White, Tile("d2")),
      Tile("e2") -> new Pawn(5, Color.White, Tile("e2")),
      Tile("f2") -> new Pawn(6, Color.White, Tile("f2")),
      Tile("g2") -> new Pawn(7, Color.White, Tile("g2")),
      Tile("h2") -> new Pawn(8, Color.White, Tile("h2")),

      Tile("a8") -> new Rook(1, Color.Black, Tile("a8")),
      Tile("b8") -> new Knight(1, Color.Black, Tile("b8")),
      Tile("c8") -> new Bishop(1, Color.Black, Tile("c8")),
      Tile("d8") -> new Queen( Color.Black, Tile("d8")),
      Tile("e8") -> new King( Color.Black, Tile("e8")),
      Tile("f8") -> new Bishop(2, Color.Black, Tile("f8")),
      Tile("g8") -> new Knight(2, Color.Black, Tile("g8")),
      Tile("h8") -> new Rook(2, Color.Black, Tile("h8")),
      Tile("a7") -> new Pawn(1, Color.Black, Tile("a7")),
      Tile("b7") -> new Pawn(2, Color.Black, Tile("b7")),
      Tile("c7") -> new Pawn(3, Color.Black, Tile("c7")),
      Tile("d7") -> new Pawn(4, Color.Black, Tile("d7")),
      Tile("e7") -> new Pawn(5, Color.Black, Tile("e7")),
      Tile("f7") -> new Pawn(6, Color.Black, Tile("f7")),
      Tile("g7") -> new Pawn(7, Color.Black, Tile("g7")),
      Tile("h7") -> new Pawn(8, Color.Black, Tile("h7")),
    )

    def getInitState:State = new State(tiles)
  }

  case class Tile(id: String) {

    def this(x: Int, y: Int) = {
      this(('a' - 1 + x).toChar.toString + y)
    }

    def validate: Try[Tile] = {
      if (id.matches("[a-hA-H]+[1-8]+")) Success(this) else Failure(new Exception(s"Invalid tile [$id]"))
    }
    def getX:Int = id.toLowerCase.charAt(0) - 'a' + 1
    def getY:Int = id.charAt(1) - '1' + 1

    def move(x: Int, y: Int): Option[Tile] = {
      val newX = getX + x
      val newY = getY + y

      if (newX > 0 && newX <= 8 && newY > 0 && newY <= 8) Some(new Tile(newX, newY)) else None
    }
  }

  def apply(state: State):Option[Board] = {
    state.validate.fold(error=> {
      error.foreach(_.printStackTrace())
      None
    }, _=> Some(new Board(state)))
  }

}