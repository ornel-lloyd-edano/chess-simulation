package chess.domain.pieces

import chess.domain.Board.Tile
import chess.domain.MutableBoardState
import chess.domain.pieces.ChessPiece.{ChessPieceType, Color}

case class Knight(id: Int, override val color: Color, override val tile: Tile) extends ChessPiece(ChessPieceType.Knight, color, tile) {

  override def getCapturingMoves(implicit boardState: MutableBoardState): Seq[Tile] = {
    Seq(
      tile.move(-1, 2),
      tile.move(1, 2),
      tile.move(-2, 1),
      tile.move(-2, -1),
      tile.move(-1, -2),
      tile.move(1, -2),
      tile.move(2, -1),
      tile.move(2, 1)
    ).flatten
  }
  override def getAllMoves(implicit boardState: MutableBoardState): Seq[Tile] = {
    getCapturingMoves(boardState).filterNot(tile=> boardState.tiles.find {
      case (occupiedTile, chessPiece)=>
        tile == occupiedTile && chessPiece.color == this.color
    }.isDefined)
  }
  override def isBlocked(validDestination: Tile)(implicit boardState: MutableBoardState): Boolean = false
  override def clone(tile: Tile): ChessPiece = this.copy(tile = tile)
}
