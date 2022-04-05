package chess

import chess.Board.{State, Tile}
import chess.ChessPiece.Color
import chess.pieces.{Bishop, Knight, Pawn}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class KnightSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "Knight" should {
    "be able to move in any L-shaped fashion (3 tiles total)" in {
      Given("black knight at tile d5, two black pawns at tile c7 and e7 are removed to give space")
      val initState = State.getInitState
      initState.set(Knight(1, Color.Black, Tile("b8")), Tile("d5")).isSuccess mustBe true
      initState.removeAt(Tile("c7")).isSuccess mustBe true
      initState.removeAt(Tile("e7")).isSuccess mustBe true

      val board = Board(initState)
      board.isDefined mustBe true

      When("black knight from tile d5 moves 2 tiles up and 1 tile left")
      val blackKnight = board.flatMap(_.get(Tile("d5")))
      blackKnight mustBe Some(Knight(1, Color.Black, Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("c7")).toOption)).isDefined mustBe true

      Then("black knight should be in tile c7")
      board.flatMap(_.get(Tile("c7"))) mustBe Some(Knight(1, Color.Black, Tile("c7")))

      When("black knight returns to d5 then moves 2 tiles up and 1 tile right")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("c7")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("e7")).toOption)).isDefined mustBe true

      Then("black knight should be in tile e7")
      board.flatMap(_.get(Tile("e7"))) mustBe Some(Knight(1, Color.Black, Tile("e7")))

      When("black knight returns to d5 then moves 2 tiles left and 1 tile up")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("e7")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("b6")).toOption)).isDefined mustBe true

      Then("black knight should be in tile b6")
      board.flatMap(_.get(Tile("b6"))) mustBe Some(Knight(1, Color.Black, Tile("b6")))

      When("black knight returns to d5 then moves 2 tiles left and 1 tile down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("b6")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("b4")).toOption)).isDefined mustBe true

      Then("black knight should be in tile b4")
      board.flatMap(_.get(Tile("b4"))) mustBe Some(Knight(1, Color.Black, Tile("b4")))

      When("black knight returns to d5 then moves 1 tile left and 2 tiles down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("b4")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("c3")).toOption)).isDefined mustBe true

      Then("black knight should be in tile c3")
      board.flatMap(_.get(Tile("c3"))) mustBe Some(Knight(1, Color.Black, Tile("c3")))

      When("black knight returns to d5 then moves 1 tile right and 2 tiles down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("c3")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("e3")).toOption)).isDefined mustBe true

      Then("black knight should be in tile e3")
      board.flatMap(_.get(Tile("e3"))) mustBe Some(Knight(1, Color.Black, Tile("e3")))

      When("black knight returns to d5 then moves 2 tiles right and 1 tile down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("e3")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("f4")).toOption)).isDefined mustBe true

      Then("black knight should be in tile f4")
      board.flatMap(_.get(Tile("f4"))) mustBe Some(Knight(1, Color.Black, Tile("f4")))

      When("black knight returns to d5 then moves 2 tiles right and 1 tile up")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("f4")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("f6")).toOption)).isDefined mustBe true

      Then("black knight should be in tile f6")
      board.flatMap(_.get(Tile("f6"))) mustBe Some(Knight(1, Color.Black, Tile("f6")))
    }

    "be able to move in any L-shaped fashion and jump over enemy and friendly chess pieces" in {
      Given("black knight at tile d5, surrounded by white and black pawns")
      val initState = State.getInitState
      initState.set(Knight(1, Color.Black, Tile("b8")), Tile("d5")).isSuccess mustBe true
      initState.set(Pawn(2, Color.Black, Tile("b7")), Tile("c6")).isSuccess mustBe true
      initState.set(Pawn(3, Color.Black, Tile("c7")), Tile("c5")).isSuccess mustBe true
      initState.set(Pawn(4, Color.Black, Tile("d7")), Tile("d6")).isSuccess mustBe true
      initState.set(Pawn(5, Color.Black, Tile("e7")), Tile("e6")).isSuccess mustBe true
      initState.set(Pawn(6, Color.Black, Tile("f7")), Tile("f5")).isSuccess mustBe true

      initState.set(Pawn(2, Color.White, Tile("b2")), Tile("c4")).isSuccess mustBe true
      initState.set(Pawn(3, Color.White, Tile("c2")), Tile("c3")).isSuccess mustBe true
      initState.set(Pawn(4, Color.White, Tile("d2")), Tile("d3")).isSuccess mustBe true
      initState.set(Pawn(5, Color.White, Tile("e2")), Tile("e3")).isSuccess mustBe true
      initState.set(Pawn(6, Color.White, Tile("f2")), Tile("e4")).isSuccess mustBe true

      val board = Board(initState)
      board.isDefined mustBe true

      When("black knight from tile d5 moves 2 tiles up and 1 tile left")
      val blackKnight = board.flatMap(_.get(Tile("d5")))
      blackKnight mustBe Some(Knight(1, Color.Black, Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("c7")).toOption)).isDefined mustBe true

      Then("black knight should be in tile c7")
      board.flatMap(_.get(Tile("c7"))) mustBe Some(Knight(1, Color.Black, Tile("c7")))

      When("black knight returns to d5 then moves 2 tiles up and 1 tile right")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("c7")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("e7")).toOption)).isDefined mustBe true

      Then("black knight should be in tile e7")
      board.flatMap(_.get(Tile("e7"))) mustBe Some(Knight(1, Color.Black, Tile("e7")))

      When("black knight returns to d5 then moves 2 tiles left and 1 tile up")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("e7")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("b6")).toOption)).isDefined mustBe true

      Then("black knight should be in tile b6")
      board.flatMap(_.get(Tile("b6"))) mustBe Some(Knight(1, Color.Black, Tile("b6")))

      When("black knight returns to d5 then moves 2 tiles left and 1 tile down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("b6")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("b4")).toOption)).isDefined mustBe true

      Then("black knight should be in tile b4")
      board.flatMap(_.get(Tile("b4"))) mustBe Some(Knight(1, Color.Black, Tile("b4")))

      When("black knight returns to d5 then moves 1 tile left and 2 tiles down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("b4")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("c3")).toOption)).isDefined mustBe true

      Then("black knight should be in tile c3")
      board.flatMap(_.get(Tile("c3"))) mustBe Some(Knight(1, Color.Black, Tile("c3")))

      When("black knight returns to d5 then moves 1 tile right and 2 tiles down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("c3")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("e3")).toOption)).isDefined mustBe true

      Then("black knight should be in tile e3")
      board.flatMap(_.get(Tile("e3"))) mustBe Some(Knight(1, Color.Black, Tile("e3")))

      When("black knight returns to d5 then moves 2 tiles right and 1 tile down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("e3")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("f4")).toOption)).isDefined mustBe true

      Then("black knight should be in tile f4")
      board.flatMap(_.get(Tile("f4"))) mustBe Some(Knight(1, Color.Black, Tile("f4")))

      When("black knight returns to d5 then moves 2 tiles right and 1 tile up")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("f4")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("f6")).toOption)).isDefined mustBe true

      Then("black knight should be in tile f6")
      board.flatMap(_.get(Tile("f6"))) mustBe Some(Knight(1, Color.Black, Tile("f6")))
    }

    "be able to move in any L-shaped fashion and capture enemy chess pieces at destination tile" in {
      Given("black knight at tile d5, white pawns at b4, c3, e3, f4")
      val initState = State.getInitState
      initState.set(Knight(1, Color.Black, Tile("b8")), Tile("d5")).isSuccess mustBe true
      initState.set(Pawn(2, Color.White, Tile("b2")), Tile("b4")).isSuccess mustBe true
      initState.set(Pawn(3, Color.White, Tile("c2")), Tile("c3")).isSuccess mustBe true
      initState.set(Pawn(5, Color.White, Tile("e2")), Tile("e3")).isSuccess mustBe true
      initState.set(Pawn(6, Color.White, Tile("f2")), Tile("f4")).isSuccess mustBe true

      val board = Board(initState)
      board.isDefined mustBe true

      When("black knight at d5 moves 2 tiles left and 1 tile down")
      val blackKnight = board.flatMap(_.get(Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("b4")).toOption)).isDefined mustBe true

      Then("white pawn captured, black knight should be in tile b4")
      board.flatMap(_.get(Pawn(2, Color.White, Tile("b4"), true))) mustBe None
      board.flatMap(_.get(Tile("b4"))) mustBe Some(Knight(1, Color.Black, Tile("b4")))

      When("black knight returns to d5 then moves 1 tile left and 2 tiles down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("b4")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("c3")).toOption)).isDefined mustBe true

      Then("white pawn captured, black knight should be in tile c3")
      board.flatMap(_.get(Pawn(3, Color.White, Tile("c3"), true))) mustBe None
      board.flatMap(_.get(Tile("c3"))) mustBe Some(Knight(1, Color.Black, Tile("c3")))

      When("black knight returns to d5 then moves 1 tile right and 2 tiles down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("c3")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("e3")).toOption)).isDefined mustBe true

      Then("white pawn captured, black knight should be in tile e3")
      board.flatMap(_.get(Pawn(5, Color.White, Tile("e3"), true))) mustBe None
      board.flatMap(_.get(Tile("e3"))) mustBe Some(Knight(1, Color.Black, Tile("e3")))

      When("black knight returns to d5 then moves 2 tiles right and 1 tile down")
      board.flatMap(_.set(Knight(1, Color.Black, Tile("e3")), Tile("d5")).toOption).isDefined mustBe true
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("f4")).toOption)).isDefined mustBe true

      Then("white pawn captured, black knight should be in tile f4")
      board.flatMap(_.get(Pawn(6, Color.White, Tile("f4"), true))) mustBe None
      board.flatMap(_.get(Tile("f4"))) mustBe Some(Knight(1, Color.Black, Tile("f4")))
    }

    "not be able to move in any L-shaped fashion if friendlies are at destination tile" in {
      Given("black knight at tile d5, black pawns at b5, c6, e6, f5")
      val initState = State.getInitState
      initState.set(Knight(1, Color.Black, Tile("b8")), Tile("d5")).isSuccess mustBe true
      initState.set(Pawn(2, Color.Black, Tile("b7")), Tile("b5")).isSuccess mustBe true
      initState.set(Pawn(3, Color.Black, Tile("c7")), Tile("c6")).isSuccess mustBe true
      initState.set(Pawn(5, Color.Black, Tile("e7")), Tile("e6")).isSuccess mustBe true
      initState.set(Pawn(6, Color.Black, Tile("f7")), Tile("f5")).isSuccess mustBe true

      val board = Board(initState)
      board.isDefined mustBe true

      When("black knight at d5 moves 2 tiles left and 1 tile up")
      val blackKnight = board.flatMap(_.get(Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("b5")).toOption)) mustBe None

      Then("black pawn still occupies b5, black knight remains in tile d5")
      board.flatMap(_.get(Tile("b5"))) mustBe Some(Pawn(2, Color.Black, Tile("b5"), true))
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))

      When("black knight at d5 moves 1 tile left and 2 tiles up")
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("c6")).toOption)) mustBe None

      Then("black pawn still occupies c6, black knight remains in tile d5")
      board.flatMap(_.get(Tile("c6"))) mustBe Some(Pawn(3, Color.Black, Tile("c6"), true))
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))

      When("black knight at d5 moves 1 tile right and 2 tiles up")
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("e6")).toOption)) mustBe None

      Then("black pawn still occupies e6, black knight remains in tile d5")
      board.flatMap(_.get(Tile("e6"))) mustBe Some(Pawn(5, Color.Black, Tile("e6"), true))
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))

      When("black knight at d5 moves 2 tiles right and 1 tile up")
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("f5")).toOption)) mustBe None

      Then("black pawn still occupies f5, black knight remains in tile d5")
      board.flatMap(_.get(Tile("f5"))) mustBe Some(Pawn(6, Color.Black, Tile("f5"), true))
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))
    }

    "not able to move in pure horizontal or vertical direction" in {
      Given("black knight at tile d5, white pawn at d2 removed for black knight movement space")
      val initState = State.getInitState
      initState.set(Knight(1, Color.Black, Tile("b8")), Tile("d5")).isSuccess mustBe true
      initState.removeAt(Tile("d2")).isSuccess mustBe true

      val board = Board(initState)
      board.isDefined mustBe true

      When("black knight from tile d5 moves 3 tiles right")
      val blackKnight = board.flatMap(_.get(Tile("d5")))
      blackKnight mustBe Some(Knight(1, Color.Black, Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("g5")).toOption)) mustBe None

      Then("black knight should still be in tile d5")
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))
      board.flatMap(_.get(Tile("g5"))) mustBe None

      When("black knight from tile d5 moves 3 tiles left")
      blackKnight mustBe Some(Knight(1, Color.Black, Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("a5")).toOption)) mustBe None

      Then("black knight should still be in tile d5")
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))
      board.flatMap(_.get(Tile("a5"))) mustBe None

      When("black knight from tile d5 moves 3 tiles down")
      blackKnight mustBe Some(Knight(1, Color.Black, Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("d2")).toOption)) mustBe None

      Then("black knight should still be in tile d5")
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))
      board.flatMap(_.get(Tile("d2"))) mustBe None
    }

    "unable to move pure diagonal direction" in {
      Given("black knight at tile d5, black pawn at b7 and f7 removed for black knight movement space")
      val initState = State.getInitState
      initState.set(Knight(1, Color.Black, Tile("b8")), Tile("d5")).isSuccess mustBe true
      initState.removeAt(Tile("f7")).isSuccess mustBe true
      initState.removeAt(Tile("b7")).isSuccess mustBe true

      val board = Board(initState)
      board.isDefined mustBe true

      When("black knight from tile d5 moves 2 tiles diagonally at 315 deg")
      val blackKnight = board.flatMap(_.get(Tile("d5")))
      blackKnight mustBe Some(Knight(1, Color.Black, Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("f3")).toOption)) mustBe None

      Then("black knight should still be in tile d5")
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))
      board.flatMap(_.get(Tile("f3"))) mustBe None

      When("black knight from tile d5 moves 2 tiles diagonally at 45 deg")
      blackKnight mustBe Some(Knight(1, Color.Black, Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("f7")).toOption)) mustBe None

      Then("black knight should still be in tile d5")
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))
      board.flatMap(_.get(Tile("f7"))) mustBe None

      When("black knight from tile d5 moves 2 tiles diagonally at 225 deg")
      blackKnight mustBe Some(Knight(1, Color.Black, Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("b3")).toOption)) mustBe None

      Then("black knight should still be in tile d5")
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))
      board.flatMap(_.get(Tile("b3"))) mustBe None

      When("black knight from tile d5 moves 2 tiles diagonally at 135 deg")
      blackKnight mustBe Some(Knight(1, Color.Black, Tile("d5")))
      blackKnight.flatMap(knight=> board.flatMap(_.set(knight, Tile("b7")).toOption)) mustBe None

      Then("black knight should still be in tile d5")
      board.flatMap(_.get(Tile("d5"))) mustBe Some(Knight(1, Color.Black, Tile("d5")))
      board.flatMap(_.get(Tile("b7"))) mustBe None
    }

  }
}
