package chess.domain

import chess.domain.Board.{State, Tile}
import chess.domain.pieces.ChessPiece

import scala.util.{Failure, Try}

case class Turn(src: Tile, dest: Tile) {
  def move(implicit boardState: State): Try[ChessPiece] = {
    boardState.tiles.get(src) match {
      case Some(chessPiece)=>
        boardState.set(chessPiece, dest)
      case None=> Failure(new Exception(s"No chess piece at tile $src"))
    }
  }
}
