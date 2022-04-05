package chess.domain

import chess.domain.Board.Tile
import chess.domain.pieces.ChessPiece

trait ReadableBoardState {
  /**
   * Gets all the tiles occupied by all uncaptured chess pieces
   * @return a table of Tile locations and the corresponding chess piece on top of it
   */
  def tiles: Map[Tile, ChessPiece]

  /**
   * Gets all the chess pieces currently occupying the tiles
   * @return a table of Chess pieces and the corresponding tile locations each occupy
   */
  def chessPieces: Map[ChessPiece, Tile]
}
