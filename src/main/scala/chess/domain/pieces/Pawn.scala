package chess.domain.pieces

import chess.domain.Board.Tile
import chess.domain.MutableBoardState
import chess.domain.pieces.ChessPiece.{ChessPieceType, Color}

case class Pawn(id: Int, override val color: Color, override val tile: Tile, hasMoved: Boolean = false) extends ChessPiece(ChessPieceType.Pawn, color, tile) {
  override def getCapturingMoves(implicit boardState: MutableBoardState): Seq[Tile] = {
    (if (color == Color.White) {
      Seq(
        tile.move(1, 1),
        tile.move(-1, 1)
      )
    } else {
      Seq(
        tile.move(1, -1),
        tile.move(-1, -1)
      )
    }).flatten.filter(tile => boardState.tiles.find { case (occupiedTile, occupyingPiece) =>
      tile == occupiedTile && occupyingPiece.color != this.color
    }.isDefined)
  }

  override def isBlocked(validDestination: Tile)(implicit boardState: MutableBoardState): Boolean = {
    if (tile.getX == validDestination.getX) {
      boardState.chessPieces.find {
        case (chessPiece, tile)=>
          tile.getY == validDestination.getY + (if (color == Color.White) -1 else 1) &&
            (tile.getX == validDestination.getX) && chessPiece != this
      }.isDefined
    } else {//diagonal capture move
      boardState.tiles.find {
        case (tile, chessPiece)=> //a chess piece of same color is capable of blocking a pawn attack move
          this.tile == tile && chessPiece.color != this.color
      }.isDefined
    }
  }

  override def getAllMoves(implicit boardState: MutableBoardState): Seq[Tile] = {
    val nonCaptureMoves = (if (!hasMoved) {
      if (color == Color.White) {
        Seq(tile.move(0, 1), tile.move(0, 2))
      } else {
        Seq(tile.move(0, -1), tile.move(0, -2))
      }
    } else {
      if (color == Color.White) {
        Seq(tile.move(0, 1))
      } else {
        Seq(tile.move(0, -1))
      }
    }).flatten.filterNot(tile => boardState.tiles.find { case (occupiedTile, _) =>
      tile == occupiedTile
    }.isDefined)

    (nonCaptureMoves ++ getCapturingMoves(boardState))

  }

  override def clone(tile: Tile): ChessPiece = this.copy(tile = tile, hasMoved = if(tile != this.tile) true else false)
}
