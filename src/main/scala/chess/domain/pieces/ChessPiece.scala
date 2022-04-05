package chess.domain.pieces

import chess.domain.Board
import chess.domain.Board.Tile

abstract class ChessPiece(val `type`: ChessPiece.ChessPieceType, val color: ChessPiece.Color, val tile: Tile) {
  /**
   * getCapturingMoves method gets all tiles available to this chess piece for capturing an enemy chess piece
   * @param boardState is the current state of the chess board, which pieces are still alive and located on which tiles
   * @return
   */
  def getCapturingMoves(implicit boardState: Board.State): Seq[Tile]

  /**
   * getAllMoves method gets all tiles available to this chess piece for both capturing or just moving normally
   * @param boardState is the current state of the chess board, which pieces are still alive and located on which tiles
   * @return a list of tiles where this chess piece can legally move according to its type, color and current location
   */
  def getAllMoves(implicit boardState: Board.State): Seq[Tile]

  /**
   * isBlocked method checks if the current chess piece can reach the given tile
   * @param validDestination is a tile that should come pre-validated from getAllMoves
   * @param boardState is the current state of the chess board, which pieces are still alive and located on which tiles
   * @return true of the destination tile is blocked and unreachable else false
   */
  def isBlocked(validDestination: Tile)(implicit boardState: Board.State): Boolean

  /**
   * clone method is used because ChessPiece is an abstract class which aims to be immutable in concrete implementation
   * A case class with built in copy method could have been used but extending a case class is a bad idea
   * @param tile the new tile location of the new chess piece clone
   * @return a new chess piece with the same properties as the old one except a new tile location
   */
  def clone(tile: Tile): ChessPiece
}

object ChessPiece {
  sealed abstract class ChessPieceType(name: String)

  object ChessPieceType {
    case object King extends ChessPieceType("King")
    case object Queen extends ChessPieceType("Queen")
    case object Bishop extends ChessPieceType("Bishop")
    case object Knight extends ChessPieceType("Knight")
    case object Rook extends ChessPieceType("Rook")
    case object Pawn extends ChessPieceType("Pawn")
  }

  sealed abstract class Color(value: String)
  object Color {
    case object White extends Color("White")
    case object Black extends Color("Black")
  }
}
