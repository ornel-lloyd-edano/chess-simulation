package chess.domain

import chess.domain.Board.{State, Tile}
import chess.domain.pieces.ChessPiece.Color
import chess.domain.pieces.{Pawn, Rook}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RookSpec extends AnyWordSpec with Matchers with GivenWhenThen {

  "Rook" should {
    "be able to move horizontally" in {
      Given("default initial arrangement except black rook from tile a8 is now at tile c4")
      val initState = State.getInitState
      initState.set(Rook(1, Color.Black, Tile("a8")), Tile("c4")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black rook from tile c4 moves 1 tile to the right")
      val blackRook = board.flatMap(_.get(Tile("c4")))
      blackRook mustBe Some(Rook(1, Color.Black, Tile("c4")))
      val blackRookInD4 = blackRook.flatMap(rook=> board.flatMap(_.set(rook, Tile("d4")).toOption))
      blackRookInD4.isDefined mustBe true

      Then("black rook should be in tile d4")
      board.flatMap(_.get(Tile("d4"))) mustBe blackRookInD4

      When("black rook from tile d4 moves 3 tiles to the right")
      val blackRookInG4 = blackRookInD4.flatMap(rook=> board.flatMap(_.set(rook, Tile("g4")).toOption))
      blackRookInG4.isDefined mustBe true

      Then("black rook should be in tile g4")
      board.flatMap(_.get(Tile("g4"))) mustBe blackRookInG4

      When("black rook from tile g4 moves 6 tiles to the left")
      val blackRookInB4 = blackRookInG4.flatMap(rook=> board.flatMap(_.set(rook, Tile("b4")).toOption))
      blackRookInB4.isDefined mustBe true

      Then("black rook should be in tile b4")
      board.flatMap(_.get(Tile("b4"))) mustBe blackRookInB4
    }

    "be able to move vertically" in {
      Given("default initial arrangement except black rook from tile a8 is now at tile c6")
      val initState = State.getInitState
      initState.set(Rook(1, Color.Black, Tile("a8")), Tile("c6")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black rook from tile c6 moves 1 tile down")
      val blackRook = board.flatMap(_.get(Tile("c6")))
      blackRook mustBe Some(Rook(1, Color.Black, Tile("c6")))
      val blackRookInC5 = blackRook.flatMap(rook=> board.flatMap(_.set(rook, Tile("c5")).toOption))
      blackRookInC5.isDefined mustBe true

      Then("black rook should be in tile c5")
      board.flatMap(_.get(Tile("c5"))) mustBe blackRookInC5

      When("black rook from tile c5 moves 2 tiles down")
      val blackRookInC3 = blackRookInC5.flatMap(rook=> board.flatMap(_.set(rook, Tile("c3")).toOption))
      blackRookInC3.isDefined mustBe true

      Then("black rook should be in tile c3")
      board.flatMap(_.get(Tile("c3"))) mustBe blackRookInC3

      When("black rook from tile c3 moves 3 tiles up")
      val blackRookInC6 = blackRookInC3.flatMap(rook=> board.flatMap(_.set(rook, Tile("c6")).toOption))
      blackRookInC6.isDefined mustBe true

      Then("black rook should be in tile c6")
      board.flatMap(_.get(Tile("c6"))) mustBe blackRookInC6
    }

    "not able to move diagonally" in {
      Given("default initial arrangement except black rook from tile a8 is now at tile c6")
      val initState = State.getInitState
      initState.set(Rook(1, Color.Black, Tile("a8")), Tile("c6")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black rook from tile c6 moves two tiles right and two tiles down to f4")
      val blackRook = board.flatMap(_.get(Tile("c6")))
      blackRook mustBe Some(Rook(1, Color.Black, Tile("c6")))
      val blackRookInF4 = blackRook.flatMap(rook=> board.flatMap(_.set(rook, Tile("f4")).toOption))
      blackRookInF4.isDefined mustBe false

      Then("black rook should still be in tile c6")
      board.flatMap(_.get(Tile("c6"))) mustBe blackRook
    }

    "not be able to go to destination tile if friendly is blocking" in {
      Given("black rook from tile a8 is now at tile b4, black pawn from d7 is at d5")
      val initState = State.getInitState
      initState.set(Rook(1, Color.Black, Tile("a8")), Tile("b5")).isSuccess mustBe true
      initState.set(Pawn(2, Color.Black, Tile("b7")), Tile("b4")).isSuccess mustBe true
      initState.set(Pawn(4, Color.Black, Tile("d7")), Tile("d5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black rook from tile b5 moves right to tile g5")
      val blackRook = board.flatMap(_.get(Tile("b5")))
      blackRook mustBe Some(Rook(1, Color.Black, Tile("b5")))
      val blackRookInG5 = blackRook.flatMap(rook=> board.flatMap(_.set(rook, Tile("g5")).toOption))

      Then("black rook should still be in tile b5 and not in tile g5")
      blackRookInG5 mustBe None
      board.flatMap(_.get(Tile("g5"))) mustBe None
      board.flatMap(_.get(Tile("b5"))) mustBe blackRook

      When("black rook from tile b5 moves down to tile b3")
      board.flatMap(_.get(Tile("b5"))) mustBe blackRook
      val blackRookInB3 = blackRook.flatMap(rook=> board.flatMap(_.set(rook, Tile("b3")).toOption))
      blackRookInB3 mustBe None

      Then("black rook should still be in tile b5 and not in tile b3")
      blackRookInB3 mustBe None
      board.flatMap(_.get(Tile("b3"))) mustBe None
      board.flatMap(_.get(Tile("b5"))) mustBe blackRook
    }

    "not be able to go to destination tile if enemy is blocking" in {
      Given("black rook from tile a8 is now at tile b4, white pawn from d2 is at d5")
      val initState = State.getInitState
      initState.set(Rook(1, Color.Black, Tile("a8")), Tile("b5")).isSuccess mustBe true
      initState.set(Pawn(4, Color.White, Tile("d2")), Tile("d5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black rook from tile b5 moves to tile g5")
      val blackRook = board.flatMap(_.get(Tile("b5")))
      blackRook mustBe Some(Rook(1, Color.Black, Tile("b5")))
      val blackRookInG5 = blackRook.flatMap(rook=> board.flatMap(_.set(rook, Tile("g5")).toOption))

      Then("black rook should still be in tile b5 and not in tile g5")
      blackRookInG5 mustBe None
      board.flatMap(_.get(Tile("g5"))) mustBe None
      board.flatMap(_.get(Tile("b5"))) mustBe blackRook
    }

    "not be able to go to destination tile if friendly is occupying that tile" in {
      Given("black rook from tile a8 is now at tile b4, black pawn from d7 is at d5")
      val initState = State.getInitState
      initState.set(Rook(1, Color.Black, Tile("a8")), Tile("b5")).isSuccess mustBe true
      initState.set(Pawn(6, Color.Black, Tile("f7")), Tile("f5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black rook from tile b5 moves to tile f5")
      val blackRook = board.flatMap(_.get(Tile("b5")))
      blackRook mustBe Some(Rook(1, Color.Black, Tile("b5")))
      val blackRookInF5 = blackRook.flatMap(rook=> board.flatMap(_.set(rook, Tile("f5")).toOption))

      Then("black rook should still be in tile b5 and not in tile f5")
      blackRookInF5 mustBe None
      board.flatMap(_.get(Tile("f5"))) mustBe Some(Pawn(6, Color.Black, Tile("f5"), true))
      board.flatMap(_.get(Tile("b5"))) mustBe blackRook
    }

    "be able to go to destination tile and capture enemy occupying that tile" in {
      Given("black rook from tile a8 is now at tile b4, black pawn from d7 is at d5")
      val initState = State.getInitState
      initState.set(Rook(1, Color.Black, Tile("a8")), Tile("b5")).isSuccess mustBe true
      initState.set(Pawn(6, Color.White, Tile("f2")), Tile("f5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black rook from tile b5 moves to tile f5")
      val blackRook = board.flatMap(_.get(Tile("b5")))
      blackRook mustBe Some(Rook(1, Color.Black, Tile("b5")))
      val blackRookInF5 = blackRook.flatMap(rook=> board.flatMap(_.set(rook, Tile("f5")).toOption))

      Then("white pawn is captured, black rook should now be in tile f5")
      board.flatMap(_.get(Pawn(6, Color.White, Tile("f5")))) mustBe None
      blackRookInF5 mustBe Some(Rook(1, Color.Black, Tile("f5")))
      board.flatMap(_.get(Tile("f5"))) mustBe blackRookInF5
    }

  }
}
