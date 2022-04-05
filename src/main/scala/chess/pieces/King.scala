package chess.pieces

import chess.Board.{State, Tile}
import chess.ChessPiece.{ChessPieceType, Color}
import chess.ChessPiece

case class King(override val color: Color, override val tile: Tile) extends ChessPiece(ChessPieceType.King, color, tile) {
  private[pieces] val oneTileMoves = Seq(
    tile.move(1, 0),
    tile.move(-1, 0),
    tile.move(0, 1),
    tile.move(0, -1),
    tile.move(1, 1),
    tile.move(-1, 1),
    tile.move(1, -1),
    tile.move(-1, -1)
  )

  override def getCapturingMoves(implicit boardState: State): Seq[Tile] = {
    oneTileMoves.flatten.filterNot(isBlocked)
  }

  override def getAllMoves(implicit boardState: State): Seq[Tile] = {
    getCapturingMoves(boardState).filterNot(tile=> boardState.tiles.find {
      case (occupiedTile, chessPiece)=>
        tile == occupiedTile && chessPiece.color == this.color
    }.isDefined)
  }

  //find out if the given validDestination tile from my getCapturingMoves is not among the getCapturingMoves
  //of the enemy chess pieces because if it is then the resulting move will be a check and should be blocked
  override def isBlocked(validDestination: Tile)(implicit boardState: State): Boolean = {
    boardState.chessPieces.map(_._1).filterNot(_.color == this.color)
      .flatMap {
        case enemyKing: King => //we are doing this to avoid stack overflow since calling getCapturingMoves on King calls isBlocked vice versa
          enemyKing.oneTileMoves.flatten
        case enemyChessPiece: ChessPiece =>
          enemyChessPiece.getCapturingMoves
      }.find(_ == validDestination).isDefined
  }

  override def clone(tile: Tile): ChessPiece = this.copy(tile = tile)

}
