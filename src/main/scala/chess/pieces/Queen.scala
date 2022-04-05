package chess.pieces

import chess.Board.{State, Tile}
import chess.ChessPiece
import chess.ChessPiece.{ChessPieceType, Color}

case class Queen(override val color: Color, override val tile: Tile) extends ChessPiece(ChessPieceType.Queen, color, tile) {
  private val rook = Rook(-1, color, tile)
  private val bishop = Bishop(-1, color, tile)

  override def getCapturingMoves(implicit boardState: State): Seq[Tile] = {
    rook.getCapturingMoves ++ bishop.getCapturingMoves
  }

  override def getAllMoves(implicit boardState: State): Seq[Tile] = {
    rook.getAllMoves ++ bishop.getAllMoves
  }
  override def isBlocked(validDestination: Tile)(implicit boardState: State): Boolean = {
    rook.isBlocked(validDestination) || bishop.isBlocked(validDestination)
  }
  override def clone(tile: Tile): ChessPiece = this.copy(tile = tile)
}
