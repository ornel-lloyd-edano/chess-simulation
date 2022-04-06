package chess.domain

import chess.domain.Board.{State, Tile}
import chess.domain.pieces.ChessPiece

import scala.util.Try

trait MutableBoardState {
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

  /**
   * Sets a chess piece to a desired location, regardless if it is a valid move or not, while updating the board state
   * @param chessPiece indicated by the id, type, color and current tile location
   * @param destination the new tile location to put the chosen chess piece
   * @return a copy of the chess piece with updated current tile location if success else an exception
   */
  def set(chessPiece: ChessPiece, destination: Tile): Try[ChessPiece]

  /**
   * Clears a tile from chess piece occupancy, a.k.a. removes a chess piece from the board
   * @param tile any tile from the chess board from a1 to h8
   * @return nothing if successfully removed or was already removed, exception if attempting to remove a King
   */
  def removeAt(tile: Tile): Try[Unit]

  /**
   * Checks if the chess pieces in the game are in the correct count based on color and type
   * @return the state itself if valid or a list of exceptions if invalid state
   */
  def validate: Either[Seq[Exception], State]
}
