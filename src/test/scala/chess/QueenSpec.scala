package chess

import chess.Board.{State, Tile}
import chess.ChessPiece.Color
import chess.pieces.{Pawn, Queen}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class QueenSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Queen" should {
    "be able to move diagonally" in {
      Given("default initial arrangement except black queen from tile e8 is now at tile e5")
      val initState = State.getInitState
      initState.set(Queen(Color.Black, Tile("e8")), Tile("e5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black queen from tile e5 moves diagonally to tile c3 (225 deg)")
      val blackQueen = board.flatMap(_.get(Tile("e5")))
      blackQueen mustBe Some(Queen(Color.Black, Tile("e5")))
      val blackQueenInDC3 = blackQueen.flatMap(queen=> board.flatMap(_.set(queen, Tile("c3")).toOption))
      blackQueenInDC3.isDefined mustBe true

      Then("black queen should be in tile c3")
      board.flatMap(_.get(Tile("c3"))) mustBe blackQueenInDC3

      When("black queen from tile c3 moves diagonally to tile a5 (135 deg)")
      board.flatMap(_.get(Tile("e5"))) mustBe None
      val blackQueenInA5 = blackQueenInDC3.flatMap(queen=> board.flatMap(_.set(queen, Tile("a5")).toOption))

      Then("black queen should be in tile a5")
      board.flatMap(_.get(Tile("a5"))) mustBe Some(Queen( Color.Black, Tile("a5")))

      When("black queen from tile a5 moves diagonally to tile b6 (45 deg)")
      board.flatMap(_.get(Tile("b6"))) mustBe None
      val blackQueenInB6 = blackQueenInA5.flatMap(queen=> board.flatMap(_.set(queen, Tile("b6")).toOption))

      Then("black queen should be in tile b6")
      board.flatMap(_.get(Tile("b6"))) mustBe Some(Queen( Color.Black, Tile("b6")))

      When("black queen from tile b6 moves diagonally to tile e3 (315 deg)")
      board.flatMap(_.get(Tile("e3"))) mustBe None
      val blackQueenInE3 = blackQueenInB6.flatMap(queen=> board.flatMap(_.set(queen, Tile("e3")).toOption))

      Then("black queen should be in tile e3")
      blackQueenInE3 mustBe Some(Queen( Color.Black, Tile("e3")))
      board.flatMap(_.get(Tile("e3"))) mustBe Some(Queen( Color.Black, Tile("e3")))
    }

    "be able to move horizontally" in {
      Given("default initial arrangement except black queen from tile e8 is now at tile c4")
      val initState = State.getInitState
      initState.set(Queen(Color.Black, Tile("e8")), Tile("c4")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black queen from tile c4 moves 1 tile to the right")
      val blackQueen = board.flatMap(_.get(Tile("c4")))
      blackQueen mustBe Some(Queen(Color.Black, Tile("c4")))
      val blackQueenInD4 = blackQueen.flatMap(queen=> board.flatMap(_.set(queen, Tile("d4")).toOption))
      blackQueenInD4.isDefined mustBe true

      Then("black queen should be in tile d4")
      board.flatMap(_.get(Tile("d4"))) mustBe blackQueenInD4

      When("black queen from tile d4 moves 3 tiles to the right")
      val blackQueenInG4 = blackQueenInD4.flatMap(queen=> board.flatMap(_.set(queen, Tile("g4")).toOption))
      blackQueenInG4.isDefined mustBe true

      Then("black queen should be in tile g4")
      board.flatMap(_.get(Tile("g4"))) mustBe blackQueenInG4

      When("black queen from tile g4 moves 6 tiles to the left")
      val blackQueenInB4 = blackQueenInG4.flatMap(queen=> board.flatMap(_.set(queen, Tile("b4")).toOption))
      blackQueenInB4.isDefined mustBe true

      Then("black queen should be in tile b4")
      board.flatMap(_.get(Tile("b4"))) mustBe blackQueenInB4
    }

    "be able to move vertically" in {
      Given("default initial arrangement except black queen from tile a8 is now at tile c6")
      val initState = State.getInitState
      initState.set(Queen(Color.Black, Tile("e8")), Tile("c6")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black queen from tile c6 moves 1 tile down")
      val blackQueen = board.flatMap(_.get(Tile("c6")))
      blackQueen mustBe Some(Queen(Color.Black, Tile("c6")))
      val blackQueenInC5 = blackQueen.flatMap(queen=> board.flatMap(_.set(queen, Tile("c5")).toOption))
      blackQueenInC5.isDefined mustBe true

      Then("black queen should be in tile c5")
      board.flatMap(_.get(Tile("c5"))) mustBe blackQueenInC5

      When("black queen from tile c5 moves 2 tiles down")
      val blackQueenInC3 = blackQueenInC5.flatMap(queen=> board.flatMap(_.set(queen, Tile("c3")).toOption))
      blackQueenInC3.isDefined mustBe true

      Then("black queen should be in tile c3")
      board.flatMap(_.get(Tile("c3"))) mustBe blackQueenInC3

      When("black queen from tile c3 moves 3 tiles up")
      val blackQueenInC6 = blackQueenInC3.flatMap(queen=> board.flatMap(_.set(queen, Tile("c6")).toOption))
      blackQueenInC6.isDefined mustBe true

      Then("black queen should be in tile c6")
      board.flatMap(_.get(Tile("c6"))) mustBe blackQueenInC6
    }

    "not be able to go to destination tile if enemy or friendly is blocking" in {
      Given("black queen from tile a8 is now at tile b4, white pawn from d2 is at d5")
      val initState = State.getInitState
      initState.set(Queen(Color.Black, Tile("e8")), Tile("b5")).isSuccess mustBe true
      initState.set(Pawn(2, Color.White, Tile("b2")), Tile("b4")).isSuccess mustBe true
      initState.set(Pawn(4, Color.Black, Tile("d7")), Tile("d5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black queen from tile b5 moves horizontally right to tile g5")
      val blackQueen = board.flatMap(_.get(Tile("b5")))
      blackQueen mustBe Some(Queen(Color.Black, Tile("b5")))
      val blackQueenInG5 = blackQueen.flatMap(queen=> board.flatMap(_.set(queen, Tile("g5")).toOption))

      Then("black queen should still be in tile b5, tile g5 is blocked by black pawn in d5")
      blackQueenInG5 mustBe None
      board.flatMap(_.get(Tile("g5"))) mustBe None
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Pawn(4, Color.Black, Tile("d5"), true))
      board.flatMap(_.get(Tile("b5"))) mustBe blackQueen

      When("black queen from tile b5 moves vertically down to tile b3")
      val blackQueenInB3 = blackQueen.flatMap(queen=> board.flatMap(_.set(queen, Tile("b3")).toOption))

      Then("black queen should still be in tile b5, tile b3 is blocked by white pawn in b4")
      blackQueenInB3 mustBe None
      board.flatMap(_.get(Tile("b3"))) mustBe None
      board.flatMap(_.get(Tile("b4"))) mustBe Some(Pawn(2, Color.White, Tile("b4"), true))
      board.flatMap(_.get(Tile("b5"))) mustBe blackQueen
    }

    "able to capture enemy located on tiles along diagonal movement" in {
      Given("black queen at tile e6, white pawn at tile c4")
      val initState = State.getInitState
      initState.set(Queen(Color.Black, Tile("e8")), Tile("e6")).isSuccess mustBe true
      initState.set(Pawn(3, Color.White, Tile("c2")), Tile("c4")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black queen from tile e6 moves vertically to tile c4")
      val blackQueen = board.flatMap(_.get(Tile("e6")))
      blackQueen mustBe Some(Queen(Color.Black, Tile("e6")))
      val blackQueenInC4 = blackQueen.flatMap(queen=> board.flatMap(_.set(queen, Tile("c4")).toOption))

      Then("white pawn in c4 is captured by black queen")
      board.flatMap(_.get(Pawn(3, Color.White, Tile("c4"), true))) mustBe None
      board.flatMap(_.get(Tile("c4"))) mustBe Some(Queen(Color.Black, Tile("c4")))
    }

    "not able to capture friendlies" in {
      Given("black queen at tile e6, white pawn at tile c4")
      val initState = State.getInitState
      initState.set(Queen(Color.Black, Tile("e8")), Tile("e6")).isSuccess mustBe true
      initState.set(Pawn(3, Color.Black, Tile("c7")), Tile("c4")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black queen from tile e6 moves vertically to tile c4")
      val blackQueen = board.flatMap(_.get(Tile("e6")))
      blackQueen mustBe Some(Queen(Color.Black, Tile("e6")))
      val blackQueenInC4 = blackQueen.flatMap(queen=> board.flatMap(_.set(queen, Tile("c4")).toOption))

      Then("black pawn in c4 cannot be captured by black queen")
      blackQueenInC4 mustBe None
      board.flatMap(_.get(Tile("c4"))) mustBe Some(Pawn(3, Color.Black, Tile("c4"), true))
    }

  }
}
