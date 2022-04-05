package chess.domain.pieces

import chess.domain.Board
import chess.domain.Board.{State, Tile}
import chess.domain.pieces.ChessPiece.{ChessPieceType, Color}

case class Bishop(id: Int, override val color: Color, override val tile: Tile) extends ChessPiece(ChessPieceType.Bishop, color, tile) {
  private val moves45Deg = Seq(
    tile.move(1, 1),
    tile.move(2, 2),
    tile.move(3, 3),
    tile.move(4, 4),
    tile.move(5, 5),
    tile.move(6, 6),
    tile.move(7, 7),
    tile.move(8, 8)
  )
  private val moves225Deg = Seq(
    tile.move(-1, -1),
    tile.move(-2, -2),
    tile.move(-3, -3),
    tile.move(-4, -4),
    tile.move(-5, -5),
    tile.move(-6, -6),
    tile.move(-7, -7),
    tile.move(-8, -8)
  )
  private val moves315Deg = Seq(
    tile.move(1, -1),
    tile.move(2, -2),
    tile.move(3, -3),
    tile.move(4, -4),
    tile.move(5, -5),
    tile.move(6, -6),
    tile.move(7, -7),
    tile.move(8, -8)
  )
  private val moves135Deg = Seq(
    tile.move(-1, 1),
    tile.move(-2, 2),
    tile.move(-3, 3),
    tile.move(-4, 4),
    tile.move(-5, 5),
    tile.move(-6, 6),
    tile.move(-7, 7),
    tile.move(-8, 8)
  )

  override def getCapturingMoves(implicit boardState: Board.State): Seq[Tile] = {
    (moves45Deg ++ moves135Deg ++ moves225Deg ++ moves315Deg).flatten.filterNot(isBlocked)
  }

  override def getAllMoves(implicit boardState: State): Seq[Tile] = {
    getCapturingMoves(boardState).filterNot(tile=> boardState.tiles.find {
      case (occupiedTile, chessPiece)=>
        tile == occupiedTile && chessPiece.color == this.color
    }.isDefined)
  }

  override def isBlocked(validDestination: Tile)(implicit boardState: Board.State): Boolean = {
    val currentTileToDest =
      if (tile.getX < validDestination.getX && tile.getY < validDestination.getY) {
        moves45Deg.flatten.filter(tile=> tile.getX < validDestination.getX && tile.getY < validDestination.getY)

      } else if (tile.getX > validDestination.getX && tile.getY < validDestination.getY) {
        moves135Deg.flatten.filter(tile=> tile.getX > validDestination.getX && tile.getY < validDestination.getY)

      } else if (tile.getX > validDestination.getX && tile.getY > validDestination.getY) {
        moves225Deg.flatten.filter(tile=> tile.getX > validDestination.getX && tile.getY > validDestination.getY)

      } else {
        moves315Deg.flatten.filter(tile=> tile.getX < validDestination.getX && tile.getY > validDestination.getY)
      }

    currentTileToDest.find(boardState.tiles.contains(_)).isDefined
  }
  override def clone(tile: Tile): ChessPiece = this.copy(tile = tile)
}
