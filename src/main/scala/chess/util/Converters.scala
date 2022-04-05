package chess.util

import chess.domain.Board.Tile
import chess.domain.Turn

import scala.util.Try

object Converters {

  implicit class ArrayIntExtension(arg: Array[Int]) {
    def asTurn: Try[Turn] = {
      val expectedInput = arg.zipWithIndex.foldLeft("") {
        case (converted, (element, idx)) if (idx + 1) % 2 == 1 =>
          converted + (97 + element).toChar
        case (converted, (element, idx)) if (idx + 1) % 2 == 0 =>
          converted + (56 - element ).toChar
      }
      val (t1, t2) = expectedInput.splitAt(2)
      Tile(t1).validate.flatMap(t1=> Tile(t2).validate.map(t2=>  Turn(src = t1, dest = t2)) )
    }
  }

}
