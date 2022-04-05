package chess.domain.pieces

import chess.domain.Board.{State, Tile}
import chess.domain.pieces.ChessPiece.{ChessPieceType, Color}

case class Rook(id: Int, override val color: Color, override val tile: Tile) extends ChessPiece(ChessPieceType.Rook, color, tile) {

  private val rightMoves = Seq(
    tile.move(1, 0),
    tile.move(2, 0),
    tile.move(3, 0),
    tile.move(4, 0),
    tile.move(5, 0),
    tile.move(6, 0),
    tile.move(7, 0),
    tile.move(8, 0)
  )
  private val leftMoves = Seq(
    tile.move(-1, 0),
    tile.move(-2, 0),
    tile.move(-3, 0),
    tile.move(-4, 0),
    tile.move(-5, 0),
    tile.move(-6, 0),
    tile.move(-7, 0),
    tile.move(-8, 0)
  )
  private val upMoves = Seq(
    tile.move(0, 1),
    tile.move(0, 2),
    tile.move(0, 3),
    tile.move(0, 4),
    tile.move(0, 5),
    tile.move(0, 6),
    tile.move(0, 7),
    tile.move(0, 8)
  )
  private val downMoves = Seq(
    tile.move(0, -1),
    tile.move(0, -2),
    tile.move(0, -3),
    tile.move(0, -4),
    tile.move(0, -5),
    tile.move(0, -6),
    tile.move(0, -7),
    tile.move(0, -8)
  )

  override def getCapturingMoves(implicit boardState: State): Seq[Tile] = {
    (rightMoves ++ leftMoves ++ upMoves ++ downMoves).flatten.filterNot(isBlocked)
  }
  override def getAllMoves(implicit boardState: State): Seq[Tile] = {
    getCapturingMoves(boardState).filterNot(tile=> boardState.tiles.find {
      case (occupiedTile, chessPiece)=>
        tile == occupiedTile && chessPiece.color == this.color
    }.isDefined)
  }
  override def isBlocked(validDestination: Tile)(implicit boardState: State): Boolean = {
    val currentTileToDest = if (tile.getY == validDestination.getY && tile.getX < validDestination.getX) { //check horizontal-right direction
      rightMoves.flatten.filter(_.getX < validDestination.getX)
    } else if (tile.getY == validDestination.getY && tile.getX > validDestination.getX) { //check horizontal-left direction
      leftMoves.flatten.filter(_.getX > validDestination.getX)
    } else if (tile.getX == validDestination.getX && tile.getY < validDestination.getY) { //check vertical-up direction
      upMoves.flatten.filter(_.getY < validDestination.getY)
    } else { //check vertical-down direction
      downMoves.flatten.filter(_.getY > validDestination.getY)
    }

    currentTileToDest.find(boardState.tiles.contains(_)).isDefined
  }
  override def clone(tile: Tile): ChessPiece = this.copy(tile = tile)
}
