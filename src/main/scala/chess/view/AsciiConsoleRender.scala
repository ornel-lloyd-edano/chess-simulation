package chess.view

import chess.domain.ReadableBoardState
import chess.domain.pieces.ChessPiece.{ChessPieceType, Color}

class AsciiConsoleRender(state: ReadableBoardState) extends BoardStateRenderer {
  private val `isWhite` = true
  private val `isBlack` = false
  private val `isKnight` = false
  private val `notKnight` = true
  private val oneSpace = " "

  val template =
    """
       |*******************************|
       | a8| b8| c8| d8| e8| f8| g8| h8|
       |*******************************|
       | a7| b7| c7| d7| e7| f7| g7| h7|
       |*******************************|
       | a6| b6| c6| d6| e6| f6| g6| h6|
       |*******************************|
       | a5| b5| c5| d5| e5| f5| g5| h5|
       |*******************************|
       | a4| b4| c4| d4| e4| f4| g4| h4|
       |*******************************|
       | a3| b3| c3| d3| e3| f3| g3| h3|
       |*******************************|
       | a2| b2| c2| d2| e2| f2| g2| h2|
       |*******************************|
       | a1| b1| c1| d1| e1| f1| g1| h1|
       |*******************************|""".stripIndent().stripLineEnd

  def toAscii(): String = {
    val labels = state.tiles.map {
      case (tile, chessPiece)=>
        val chessPieceSymbol = (chessPiece.color == Color.White, chessPiece.`type` != ChessPieceType.Knight) match {
          case (`isWhite`, `notKnight`)  =>
            chessPiece.`type`.toString.toUpperCase.charAt(0)
          case (`isWhite`, `isKnight`)  =>
            chessPiece.`type`.toString.toUpperCase.charAt(1)
          case (`isBlack`, `notKnight`)  =>
            chessPiece.`type`.toString.toLowerCase.charAt(0)
          case (`isBlack`, `isKnight`)  =>
            chessPiece.`type`.toString.toLowerCase.charAt(1)
        }
        (tile.id -> chessPieceSymbol)
    }
    labels.foldLeft(template) {
      case (updatedBoard, (tileId: String, chessPieceSymbol: Char))=>
        updatedBoard.replace(tileId, s"${chessPieceSymbol.toString}$oneSpace")
    }
  }

  override def renderBoard(): Unit = {
    println(System.lineSeparator())
    println(toAscii())
    println(System.lineSeparator())
  }
}
