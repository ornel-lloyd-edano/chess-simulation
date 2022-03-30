package chess

import chess.Board.Tile

case class ChessPiece(`type`: ChessPiece.ChessPieceType, color: ChessPiece.Color, tile: Tile)

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
