package chess.domain

import chess.domain.Board.{State, Tile}
import chess.domain.pieces.ChessPiece.Color
import chess.domain.pieces.{Bishop, Pawn}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BishopSpec extends AnyWordSpec with Matchers with GivenWhenThen {

  "Bishop" should {
    "be able to move diagonally" in {
      Given("default initial arrangement except black bishop from tile c8 is now at tile e5")
      val initState = State.getInitState
      initState.set(Bishop(1, Color.Black, Tile("c8")), Tile("e5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black bishop from tile e5 moves diagonally to tile c3 (225 deg)")
      val blackBishop = board.flatMap(_.get(Tile("e5")))
      blackBishop mustBe Some(Bishop(1, Color.Black, Tile("e5")))
      val blackBishopInDC3 = blackBishop.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("c3")).toOption))
      blackBishopInDC3.isDefined mustBe true

      Then("black bishop should be in tile c3")
      board.flatMap(_.get(Tile("c3"))) mustBe blackBishopInDC3

      When("black bishop from tile c3 moves diagonally to tile a5 (135 deg)")
      board.flatMap(_.get(Tile("e5"))) mustBe None
      val blackBishopInA5 = blackBishopInDC3.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("a5")).toOption))

      Then("black bishop should be in tile a5")
      board.flatMap(_.get(Tile("a5"))) mustBe Some(Bishop(1, Color.Black, Tile("a5")))

      When("black bishop from tile a5 moves diagonally to tile b6 (45 deg)")
      board.flatMap(_.get(Tile("b6"))) mustBe None
      val blackBishopInB6 = blackBishopInA5.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("b6")).toOption))

      Then("black bishop should be in tile b6")
      board.flatMap(_.get(Tile("b6"))) mustBe Some(Bishop(1, Color.Black, Tile("b6")))

      When("black bishop from tile b6 moves diagonally to tile e3 (315 deg)")
      board.flatMap(_.get(Tile("e3"))) mustBe None
      val blackBishopInE3 = blackBishopInB6.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("e3")).toOption))

      Then("black bishop should be in tile e3")
      board.flatMap(_.get(Tile("e3"))) mustBe Some(Bishop(1, Color.Black, Tile("e3")))
    }

    "not able to move horizontally" in {
      Given("default initial arrangement except black bishop from tile c8 is now at tile e5")
      val initState = State.getInitState
      initState.set(Bishop(1, Color.Black, Tile("c8")), Tile("e5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black bishop from tile e5 moves horizontally left to tile a5")
      val blackBishop = board.flatMap(_.get(Tile("e5")))
      blackBishop mustBe Some(Bishop(1, Color.Black, Tile("e5")))
      val blackBishopInA5 = blackBishop.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("a5")).toOption))

      Then("black bishop should still be in tile e5")
      blackBishopInA5 mustBe None
      board.flatMap(_.get(Tile("a5"))) mustBe None
      board.flatMap(_.get(Tile("e5"))) mustBe Some(Bishop(1, Color.Black, Tile("e5")))

      When("black bishop from tile e5 moves horizontally right to tile h5")
      val blackBishopInH5 = blackBishop.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("h5")).toOption))

      Then("black bishop should still be in tile e5")
      blackBishopInH5 mustBe None
      board.flatMap(_.get(Tile("h5"))) mustBe None
      board.flatMap(_.get(Tile("e5"))) mustBe Some(Bishop(1, Color.Black, Tile("e5")))
    }

    "not able to move vertically" in {
      Given("default initial arrangement except black bishop from tile c8 is now at tile e5")
      val initState = State.getInitState
      initState.set(Bishop(1, Color.Black, Tile("c8")), Tile("e5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black bishop from tile e5 moves vertically up to tile e6")
      val blackBishop = board.flatMap(_.get(Tile("e5")))
      blackBishop mustBe Some(Bishop(1, Color.Black, Tile("e5")))
      val blackBishopInE6 = blackBishop.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("e6")).toOption))

      Then("black bishop should still be in tile e5")
      blackBishopInE6 mustBe None
      board.flatMap(_.get(Tile("e6"))) mustBe None
      board.flatMap(_.get(Tile("e5"))) mustBe Some(Bishop(1, Color.Black, Tile("e5")))

      When("black bishop from tile e5 moves vertically down to tile e3")
      val blackBishopInE3 = blackBishop.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("e3")).toOption))

      Then("black bishop should still be in tile e5")
      blackBishopInE3 mustBe None
      board.flatMap(_.get(Tile("e3"))) mustBe None
      board.flatMap(_.get(Tile("e5"))) mustBe Some(Bishop(1, Color.Black, Tile("e5")))
    }

    "not be able to go to destination tile if friendly is blocking" in {
      Given("black bishop at tile e6, white pawn at tile c4")
      val initState = State.getInitState
      initState.set(Bishop(1, Color.Black, Tile("c8")), Tile("e6")).isSuccess mustBe true
      initState.set(Pawn(3, Color.Black, Tile("c7")), Tile("c4")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black bishop from tile e6 moves vertically to tile b3")
      val blackBishop = board.flatMap(_.get(Tile("e6")))
      blackBishop mustBe Some(Bishop(1, Color.Black, Tile("e6")))
      val blackBishopInB3 = blackBishop.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("b3")).toOption))

      Then("black bishop should still be in tile e6, tile b3 is blocked by black pawn in c4")
      blackBishopInB3 mustBe None
      board.flatMap(_.get(Tile("b3"))) mustBe None
      board.flatMap(_.get(Tile("c4"))) mustBe Some(Pawn(3, Color.Black, Tile("c4"), true))
    }

    "not be able to go to destination tile if enemy is blocking" in {
      Given("black bishop at tile e6, white pawn at tile c4")
      val initState = State.getInitState
      initState.set(Bishop(1, Color.Black, Tile("c8")), Tile("e6")).isSuccess mustBe true
      initState.set(Pawn(3, Color.White, Tile("c2")), Tile("c4")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black bishop from tile e6 moves vertically to tile b3")
      val blackBishop = board.flatMap(_.get(Tile("e6")))
      blackBishop mustBe Some(Bishop(1, Color.Black, Tile("e6")))
      val blackBishopInB3 = blackBishop.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("b3")).toOption))

      Then("black bishop should still be in tile e6, tile b3 is blocked by white pawn in c4")
      blackBishopInB3 mustBe None
      board.flatMap(_.get(Tile("b3"))) mustBe None
      board.flatMap(_.get(Tile("c4"))) mustBe Some(Pawn(3, Color.White, Tile("c4"), true))
    }

    "able to capture enemy located on tiles along diagonal movement" in {
      Given("black bishop at tile e6, white pawn at tile c4")
      val initState = State.getInitState
      initState.set(Bishop(1, Color.Black, Tile("c8")), Tile("e6")).isSuccess mustBe true
      initState.set(Pawn(3, Color.White, Tile("c2")), Tile("c4")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black bishop from tile e6 moves vertically to tile c4")
      val blackBishop = board.flatMap(_.get(Tile("e6")))
      blackBishop mustBe Some(Bishop(1, Color.Black, Tile("e6")))
      blackBishop.flatMap(bishop=> board.flatMap(_.set(bishop, Tile("c4")).toOption))

      Then("white pawn in c4 is captured by black bishop")
      board.flatMap(_.get(Pawn(3, Color.White, Tile("c4"), true))) mustBe None
      board.flatMap(_.get(Tile("c4"))) mustBe Some(Bishop(1, Color.Black, Tile("c4")))
    }
  }
}
