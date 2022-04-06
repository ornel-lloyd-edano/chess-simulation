package chess.domain.pieces

import chess.domain.Board.Tile
import chess.domain.MutableBoardState
import chess.domain.pieces.ChessPiece.{ChessPieceType, Color}

case class Queen(override val color: Color, override val tile: Tile) extends ChessPiece(ChessPieceType.Queen, color, tile) {
  private val rook = Rook(-1, color, tile)
  private val bishop = Bishop(-1, color, tile)

  override def getCapturingMoves(implicit boardState: MutableBoardState): Seq[Tile] = {
    rook.getCapturingMoves ++ bishop.getCapturingMoves
  }

  override def getAllMoves(implicit boardState: MutableBoardState): Seq[Tile] = {
    rook.getAllMoves ++ bishop.getAllMoves
  }
  override def isBlocked(validDestination: Tile)(implicit boardState: MutableBoardState): Boolean = {
    rook.isBlocked(validDestination) || bishop.isBlocked(validDestination)
  }
  override def clone(tile: Tile): ChessPiece = this.copy(tile = tile)
}
