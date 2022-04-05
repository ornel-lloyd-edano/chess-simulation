package chess.domain

import chess.domain.Board.{State, Tile}
import chess.domain.pieces.ChessPiece.Color
import chess.domain.pieces.{Bishop, King, Pawn}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class KingSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  "King" should {
    "be able to move one tile in any direction" in {
      Given("default initial arrangement except black king from tile e8 is now at tile e5")
      val initState = State.getInitState
      initState.set(King(Color.Black, Tile("e8")), Tile("e5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black king from tile e5 moves 1 tile down to e4")
      board.flatMap(_.set(King(Color.Black, Tile("e5")), Tile("e4")).toOption).isDefined mustBe true

      Then("Tile e4 now holds black king")
      board.flatMap(_.get(Tile("e5"))) mustBe None
      board.flatMap(_.get(Tile("e4"))) mustBe Some(King(Color.Black, Tile("e4")))

      When("black king from tile e4 moves 1 tile right to f4")
      board.flatMap(_.set(King(Color.Black, Tile("e4")), Tile("f4")).toOption).isDefined mustBe true

      Then("Tile f4 now holds black king")
      board.flatMap(_.get(Tile("e4"))) mustBe None
      board.flatMap(_.get(Tile("f4"))) mustBe Some(King(Color.Black, Tile("f4")))

      When("black king from tile f4 moves 1 tile up to f5")
      board.flatMap(_.set(King(Color.Black, Tile("f4")), Tile("f5")).toOption).isDefined mustBe true

      Then("Tile f5 now holds black king")
      board.flatMap(_.get(Tile("f4"))) mustBe None
      board.flatMap(_.get(Tile("f5"))) mustBe Some(King(Color.Black, Tile("f5")))

      When("black king from tile f5 moves 1 tile left to e5")
      board.flatMap(_.set(King(Color.Black, Tile("f5")), Tile("e5")).toOption).isDefined mustBe true

      Then("Tile e5 now holds black king")
      board.flatMap(_.get(Tile("f5"))) mustBe None
      board.flatMap(_.get(Tile("e5"))) mustBe Some(King(Color.Black, Tile("e5")))

      When("black king from tile e5 moves 1 tile diagonally to f4")
      board.flatMap(_.set(King(Color.Black, Tile("e5")), Tile("f4")).toOption).isDefined mustBe true

      Then("Tile f4 now holds black king")
      board.flatMap(_.get(Tile("e5"))) mustBe None
      board.flatMap(_.get(Tile("f4"))) mustBe Some(King(Color.Black, Tile("f4")))

      When("black king from tile f4 moves 1 tile diagonally to e3")
      board.flatMap(_.set(King(Color.Black, Tile("f4")), Tile("e3")).toOption).isDefined mustBe true

      Then("Tile e3 now holds black king")
      board.flatMap(_.get(Tile("f4"))) mustBe None
      board.flatMap(_.get(Tile("e3"))) mustBe Some(King(Color.Black, Tile("e3")))

      When("black king from tile e3 moves 1 tile diagonally to d4")
      board.flatMap(_.set(King(Color.Black, Tile("e3")), Tile("d4")).toOption).isDefined mustBe true

      Then("Tile d4 now holds black king")
      board.flatMap(_.get(Tile("e3"))) mustBe None
      board.flatMap(_.get(Tile("d4"))) mustBe Some(King(Color.Black, Tile("d4")))

      When("black king from tile d4 moves 1 tile diagonally to e5")
      board.flatMap(_.set(King(Color.Black, Tile("d4")), Tile("e5")).toOption).isDefined mustBe true

      Then("Tile e5 now holds black king")
      board.flatMap(_.get(Tile("d4"))) mustBe None
      board.flatMap(_.get(Tile("e5"))) mustBe Some(King(Color.Black, Tile("e5")))
    }

    "not be able to move more than 1 tile" in {
      Given("default initial arrangement except black king from tile e8 is now at tile e5")
      val initState = State.getInitState
      initState.set(King(Color.Black, Tile("e8")), Tile("e5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black king from tile e5 moves 2 tiles down to e3")
      board.flatMap(_.set(King(Color.Black, Tile("e5")), Tile("e3")).toOption) mustBe None

      Then("Tile e5 still holds black king")
      board.flatMap(_.get(Tile("e5"))) mustBe Some(King(Color.Black, Tile("e5")))

      When("black king from tile e5 moves 3 tiles left to b5")
      board.flatMap(_.set(King(Color.Black, Tile("e5")), Tile("b5")).toOption) mustBe None

      Then("Tile e5 still holds black king")
      board.flatMap(_.get(Tile("e5"))) mustBe Some(King(Color.Black, Tile("e5")))

      When("black king from tile e5 moves 3 tiles right to h5")
      board.flatMap(_.set(King(Color.Black, Tile("e5")), Tile("h5")).toOption) mustBe None

      Then("Tile e5 still holds black king")
      board.flatMap(_.get(Tile("e5"))) mustBe Some(King(Color.Black, Tile("e5")))
    }

    "be able to capture enemies 1 tile away" in {
      Given("black king from tile e8 is now at tile e5, white pawns at d5, e4, f5, white queen removed at d1")
      val initState = State.getInitState
      initState.set(King(Color.Black, Tile("e8")), Tile("e5")).isSuccess mustBe true
      initState.removeAt(Tile("d1")).isSuccess mustBe true
      initState.set(Pawn(4, Color.White, Tile("d2")), Tile("d4")).isSuccess mustBe true
      initState.set(Pawn(5, Color.White, Tile("e2")), Tile("e4")).isSuccess mustBe true
      initState.set(Pawn(6, Color.White, Tile("f2")), Tile("d5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black king from tile e5 moves 1 tile down to e4")
      board.flatMap(_.set(King(Color.Black, Tile("e5")), Tile("e4")).toOption).isDefined mustBe true

      Then("black king captures white pawn at e4")
      board.flatMap(_.get(Pawn(5, Color.White, Tile("e4"), true))) mustBe None
      board.flatMap(_.get(Tile("e4"))) mustBe Some(King(Color.Black, Tile("e4")))

      When("black king from tile e4 moves 1 tile left to d4")
      board.flatMap(_.set(King(Color.Black, Tile("e4")), Tile("d4")).toOption).isDefined mustBe true

      Then("black king captures white pawn at d4")
      board.flatMap(_.get(Pawn(4, Color.White, Tile("d4"), true))) mustBe None
      board.flatMap(_.get(Tile("d4"))) mustBe Some(King(Color.Black, Tile("d4")))

      When("black king from tile d4 moves 1 tile up to d5")
      board.flatMap(_.set(King(Color.Black, Tile("d4")), Tile("d5")).toOption).isDefined mustBe true

      Then("black king captures white pawn at d5")
      board.flatMap(_.get(Pawn(6, Color.White, Tile("d5"), true))) mustBe None
      board.flatMap(_.get(Tile("d5"))) mustBe Some(King(Color.Black, Tile("d5")))
    }

    "not able to move to tiles occupied by friendlies" in {
      Given("black king from tile e8 is now at tile e5, black pawns at d5, e4, f5")
      val initState = State.getInitState
      initState.set(King(Color.Black, Tile("e8")), Tile("e5")).isSuccess mustBe true
      initState.set(Pawn(4, Color.Black, Tile("d7")), Tile("d4")).isSuccess mustBe true
      initState.set(Pawn(5, Color.Black, Tile("e7")), Tile("e4")).isSuccess mustBe true
      initState.set(Pawn(6, Color.Black, Tile("f7")), Tile("f5")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black king from tile e5 moves 1 tile down to e4")
      board.flatMap(_.set(King(Color.Black, Tile("e5")), Tile("e4")).toOption) mustBe None

      Then("black king failed to move at e4")
      board.flatMap(_.get(Tile("e4"))) mustBe Some(Pawn(5, Color.Black, Tile("e4"), true))
      board.flatMap(_.get(Tile("e5"))) mustBe Some(King(Color.Black, Tile("e5")))

      When("black king from tile e5 moves 1 tile left to d4")
      board.flatMap(_.set(King(Color.Black, Tile("e5")), Tile("d4")).toOption) mustBe None

      Then("black king failed to move at d4")
      board.flatMap(_.get(Tile("d4"))) mustBe Some(Pawn(4, Color.Black, Tile("d4"), true))
      board.flatMap(_.get(Tile("e5"))) mustBe Some(King(Color.Black, Tile("e5")))

      When("black king from tile e5 moves 1 tile left to f5")
      board.flatMap(_.set(King(Color.Black, Tile("e5")), Tile("f5")).toOption) mustBe None

      Then("black king failed to move at f5")
      board.flatMap(_.get(Tile("f5"))) mustBe Some(Pawn(6, Color.Black, Tile("f5"), true))
      board.flatMap(_.get(Tile("e5"))) mustBe Some(King(Color.Black, Tile("e5")))
    }

    "not able to move to tiles that are guarded by enemies or capture enemies that are themselves guarded" in {
      Given("black king from tile e8 is now at tile e4, white pawns at d4, f5, bishop at h3")
      val initState = State.getInitState
      initState.set(King(Color.Black, Tile("e8")), Tile("e4")).isSuccess mustBe true
      initState.set(Pawn(4, Color.White, Tile("d2")), Tile("d4")).isSuccess mustBe true
      initState.set(Bishop(2, Color.White, Tile("f1")), Tile("h3")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black king from tile e4 captures white pawn at d4")
      board.flatMap(_.set(King(Color.Black, Tile("e4")), Tile("d4")).toOption) mustBe None

      Then("black king failed to capture white pawn at d4 (guarder by white queen)")
      board.flatMap(_.get(Tile("e4"))) mustBe Some(King(Color.Black, Tile("e4")))
      board.flatMap(_.get(Tile("d4"))) mustBe Some(Pawn(4, Color.White, Tile("d4"), true))

      When("black king from tile e4 moves 1 tile down to e3")
      board.flatMap(_.set(King(Color.Black, Tile("e4")), Tile("e3")).toOption) mustBe None

      Then("black king failed to move down to e3 (guarded by white pawn)")
      board.flatMap(_.get(Tile("e3"))) mustBe None
      board.flatMap(_.get(Tile("e4"))) mustBe Some(King(Color.Black, Tile("e4")))

      When("black king from tile e4 moves 1 tile left to f4")
      board.flatMap(_.set(King(Color.Black, Tile("e4")), Tile("f4")).toOption) mustBe None

      Then("black king failed to move left to f4 (guarded by white bishop on c1)")
      board.flatMap(_.get(Tile("f4"))) mustBe None
      board.flatMap(_.get(Tile("e4"))) mustBe Some(King(Color.Black, Tile("e4")))

      When("black king from tile e4 moves up to f5")
      board.flatMap(_.set(King(Color.Black, Tile("e4")), Tile("f5")).toOption) mustBe None

      Then("black king failed to move left to f5 (guarded by white bishop on h3)")
      board.flatMap(_.get(Tile("f5"))) mustBe None
      board.flatMap(_.get(Tile("e4"))) mustBe Some(King(Color.Black, Tile("e4")))
    }
  }
}
