package chess.view

import chess.domain.Board.Tile
import chess.domain.pieces.ChessPiece

trait BoardStateRenderer {
  def renderBoard(boardState: Map[Tile, ChessPiece]): Unit
}
