package chess

import scala.util.{Failure, Success, Try}

object Board extends App {
  
  case class Tile(id: String) {
    def validate: Try[Tile] = {
      if (id.matches("[a-hA-H]+[1-8]+")) Success(this) else Failure(new Exception(s"Invalid tile [$id]"))
    }
  }

  private val matrix = Array[Array[ChessPiece]](
    new Array[ChessPiece](8),
    new Array[ChessPiece](8),
    new Array[ChessPiece](8),
    new Array[ChessPiece](8),
    new Array[ChessPiece](8),
    new Array[ChessPiece](8),
    new Array[ChessPiece](8),
    new Array[ChessPiece](8)
  )

  def set(tile: Tile, chessPiece: ChessPiece): Try[Unit] = ???
}
