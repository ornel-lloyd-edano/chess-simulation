package chess.view

import chess.domain.Board
import chess.domain.Board.Tile
import chess.domain.pieces.ChessPiece.Color
import chess.domain.pieces.Pawn
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AsciiConsoleRendererSpec extends AnyWordSpec with Matchers with GivenWhenThen {

  "AsciiConsoleRender" should {
    "render a chess board in ascii characters" in {
      Given("default initial state of chess board")
      val asciiRenderer = new AsciiConsoleRenderer
      val state = Board.State.getInitState

      When("converted to ascii characters")
      val result = asciiRenderer.toAscii(state.tiles)

      Then("matches the expected ascii output as displayed on the console")
      val expected =
         """
           |*******************************|
           | r | n | b | q | k | b | n | r |
           |*******************************|
           | p | p | p | p | p | p | p | p |
           |*******************************|
           | a6| b6| c6| d6| e6| f6| g6| h6|
           |*******************************|
           | a5| b5| c5| d5| e5| f5| g5| h5|
           |*******************************|
           | a4| b4| c4| d4| e4| f4| g4| h4|
           |*******************************|
           | a3| b3| c3| d3| e3| f3| g3| h3|
           |*******************************|
           | P | P | P | P | P | P | P | P |
           |*******************************|
           | R | N | B | Q | K | B | N | R |
           |*******************************|""".stripIndent().stripLineEnd
      asciiRenderer.renderBoard(state.tiles)
      result mustBe expected
    }

    "update the render on state change" in {
      Given("updated initial state of chess board")
      val state = Board.State.getInitState
      val asciiRenderer = new AsciiConsoleRenderer

      state.set(Pawn(1, Color.White, Tile("a2")), Tile("a3")).isSuccess mustBe true
      state.set(Pawn(3, Color.White, Tile("c2")), Tile("c4")).isSuccess mustBe true
      state.set(Pawn(6, Color.White, Tile("f2")), Tile("f4")).isSuccess mustBe true
      state.set(Pawn(8, Color.White, Tile("h2")), Tile("h3")).isSuccess mustBe true

      When("converted to ascii characters")
      val result = asciiRenderer.toAscii(state.tiles)

      Then("matches the expected ascii output as displayed on the console")
      val expected =
        """
          |*******************************|
          | r | n | b | q | k | b | n | r |
          |*******************************|
          | p | p | p | p | p | p | p | p |
          |*******************************|
          | a6| b6| c6| d6| e6| f6| g6| h6|
          |*******************************|
          | a5| b5| c5| d5| e5| f5| g5| h5|
          |*******************************|
          | a4| b4| P | d4| e4| P | g4| h4|
          |*******************************|
          | P | b3| c3| d3| e3| f3| g3| P |
          |*******************************|
          | a2| P | c2| P | P | f2| P | h2|
          |*******************************|
          | R | N | B | Q | K | B | N | R |
          |*******************************|""".stripIndent().stripLineEnd
      asciiRenderer.renderBoard(state.tiles)
      result mustBe expected
    }
  }

}
