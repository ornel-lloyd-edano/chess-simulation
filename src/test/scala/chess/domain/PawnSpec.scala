package chess.domain

import chess.domain.Board.{State, Tile}
import chess.domain.pieces.ChessPiece.Color
import chess.domain.pieces.{Knight, Pawn}
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec


class PawnSpec extends AnyWordSpec with Matchers with GivenWhenThen {

  "White Pawn" should {
    "be able to move 1 tile upward" in {
      Given("default initial arrangement of chess pieces")
      val board = Board(State.getInitState)
      board.isDefined mustBe true

      When("white pawn from tile d2 moves 1 tile upward")
      val whitePawn = board.flatMap(_.get(Tile("d2")))
      val oneTileUpward = Tile("d3")
      val relocatedWhitePawn = whitePawn.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, oneTileUpward).toOption) )
      relocatedWhitePawn.isDefined mustBe true

      Then("white pawn should be in tile d3")
      board.flatMap(_.get(Tile("d3"))) mustBe relocatedWhitePawn
    }

    "not be able to move downward" in {
      Given("arrangement where pawn from d2 is already in d3")
      val initState = State.getInitState
      initState.set(Pawn(4, Color.White, Tile("d2")), Tile("d3")).isSuccess mustBe true

      val board = Board(initState)
      board.isDefined mustBe true

      When("white pawn from tile d3 moves 1 tile downward")
      val whitePawn = board.flatMap(_.get(Tile("d3")))
      whitePawn.isDefined mustBe true
      val oneTileDownward = Tile("d2")
      val relocatedWhitePawn = whitePawn.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, oneTileDownward).toOption) )

      Then("white pawn should be in tile d3")
      relocatedWhitePawn mustBe None
      board.flatMap(_.get(Tile("d3"))) mustBe whitePawn
    }

    "be able to move 2 tiles upward as first move only" in {
      Given("default initial arrangement of chess pieces")
      val board = Board(State.getInitState)
      board.isDefined mustBe true

      When("white pawn from tile d2 moves two tiles upward")
      val whitePawn = board.flatMap(_.get(Tile("d2")))
      val twoTilesUpward = Tile("d4")
      val relocatedWhitePawn = whitePawn.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, twoTilesUpward).toOption) )
      relocatedWhitePawn.isDefined mustBe true

      Then("white pawn should be in tile d4")
      board.flatMap(_.get(Tile("d4"))) mustBe relocatedWhitePawn
    }

    "not be able to move two tiles upward after its first move" in {
      Given("default initial arrangement of chess pieces")
      val board = Board(State.getInitState)
      board.isDefined mustBe true

      When("white pawn from tile d2 moves one tile upward")
      val whitePawn = board.flatMap(_.get(Tile("d2")))
      val oneTileUpward = Tile("d3")
      val whitePawnInD3 = whitePawn.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, oneTileUpward).toOption) )
      whitePawnInD3.isDefined mustBe true

      When("white pawn, now in d3, cannot move two tiles upward")
      val twoTileUpward = Tile("d5")
      val whitePawnInD5 = whitePawnInD3.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, twoTileUpward).toOption))

      Then("white pawn should not be in tile d5")
      board.flatMap(_.get(Tile("d5"))) mustBe None
      whitePawnInD5 mustBe None
      Then("white pawn should still be in tile d3")
      board.flatMap(_.get(Tile("d3"))) mustBe whitePawnInD3
    }

    "be able to move 1 tile upward after its first move" in {
      Given("default initial arrangement of chess pieces")
      val board = Board(State.getInitState)
      board.isDefined mustBe true

      When("white pawn from tile d2 moves one tile upward")
      val whitePawn = board.flatMap(_.get(Tile("d2")))
      whitePawn.isDefined mustBe true
      val oneTileUpward = Tile("d3")
      val whitePawnInD3 = whitePawn.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, oneTileUpward).toOption) )
      whitePawnInD3.isDefined mustBe true

      When("white pawn, now in d3, moves one tile upward")
      val oneTileUpwardAgain = Tile("d4")
      val whitePawnInD4 = whitePawnInD3.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, oneTileUpwardAgain).toOption))
      whitePawnInD4.isDefined mustBe true

      Then("white pawn should no longer be in tile d3")
      board.flatMap(_.get(Tile("d3"))) mustBe None
      Then("white pawn should not be in tile d4")
      board.flatMap(_.get(Tile("d4"))) mustBe whitePawnInD4
    }

    "not be able to move upward if friendly is blocking the way" in {
      Given("friendly knight from b1 is occupying c3")
      val initState = State.getInitState
      initState.set(Knight(1, Color.White, Tile("b1")), Tile("c3")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("white pawn from tile c2 moves 1 tile upward")
      val whitePawn = board.flatMap(_.get(Tile("c2")))
      whitePawn mustBe Some(Pawn(3, Color.White, Tile("c2")))
      val oneTileUpward = Tile("c3")
      val relocatedWhitePawn = whitePawn.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, oneTileUpward).toOption) )
      relocatedWhitePawn mustBe None

      Then("white pawn should still be in c2 because friendly knight is blocking")
      board.flatMap(_.get(Tile("c2"))) mustBe whitePawn
      board.flatMap(_.get(Tile("c3"))) mustBe Some(Knight(1, Color.White, Tile("c3")))
    }

    "not be able to move jump upward 2 tiles if friendly is blocking the first tile" in {
      Given("friendly knight from b1 is occupying c3")
      val initState = State.getInitState
      initState.set(Knight(1, Color.White, Tile("b1")), Tile("c3")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("white pawn from tile c2 moves 2 tiles upward")
      val whitePawn = board.flatMap(_.get(Tile("c2")))
      whitePawn mustBe Some(Pawn(3, Color.White, Tile("c2")))
      val twoTilesUpward = Tile("c4")
      val relocatedWhitePawn = whitePawn.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, twoTilesUpward).toOption) )
      relocatedWhitePawn mustBe None

      Then("white pawn should still be in c2 because friendly knight is blocking")
      board.flatMap(_.get(Tile("c2"))) mustBe whitePawn
      board.flatMap(_.get(Tile("c3"))) mustBe Some(Knight(1, Color.White, Tile("c3")))
    }

    "be able to move 1 tile diagonally upward when capturing" in {
      Given("2 enemy chess pieces located at tile c3 and f3")
      val initState = State.getInitState
      initState.set(Pawn(3, Color.Black, Tile("c7")), Tile("c3")).isSuccess mustBe true
      initState.set(Pawn(6, Color.Black, Tile("f7")), Tile("f3")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      val blackPawnInC3 = board.flatMap(_.get(Tile("c3")))
      blackPawnInC3.isDefined mustBe true
      val blackPawnInF3 = board.flatMap(_.get(Tile("f3")))
      blackPawnInF3.isDefined mustBe true

      When("white pawn from tile d2 moves 1 tile diagonally left upward to tile c3")
      val whitePawnInD2 = board.flatMap(_.get(Tile("d2")))
      val oneTileLeftDiagonallyUpward = Tile("c3")
      val whitePawnInC3 = whitePawnInD2.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, oneTileLeftDiagonallyUpward).toOption) )
      whitePawnInC3.isDefined mustBe true

      Then("black pawn in c3 should be captured by white pawn originally from d2")
      board.flatMap(board=> blackPawnInC3.flatMap(board.get(_))) mustBe None
      board.flatMap(_.get(Tile("c3"))) mustBe whitePawnInC3

      When("white pawn from tile e2 moves 1 tile diagonally right upward to tile f3")
      val whitePawnInE2 = board.flatMap(_.get(Tile("e2")))
      val oneTileRightDiagonallyUpward = Tile("f3")
      val whitePawnInF3 = whitePawnInE2.flatMap(whitePawn=> board.flatMap(_.set(whitePawn, oneTileRightDiagonallyUpward).toOption) )
      whitePawnInF3.isDefined mustBe true

      Then("black pawn in f3 should be captured by white pawn originally from e2")
      board.flatMap(board=> blackPawnInF3.flatMap(board.get(_))) mustBe None
      board.flatMap(_.get(Tile("f3"))) mustBe whitePawnInF3
    }

    "not able to move diagonally when not capturing" in {
      Given("default initial arrangement of chess pieces")
      val board = Board(State.getInitState)
      board.isDefined mustBe true

      When("white pawn from tile d2 move diagonally to tile c3")
      val whitePawnInD2 = board.flatMap(_.get(Tile("d2")))
      val oneTileLeftDiagonallyUpward = Tile("c3")
      val result = whitePawnInD2.flatMap(whitePawn=> board.map(_.set(whitePawn, oneTileLeftDiagonallyUpward)) )
      result.map(_.isFailure) mustBe Some(true)

      Then("white pawn still remains in d2")
      board.flatMap(_.get(Tile("d2"))) mustBe whitePawnInD2
      board.flatMap(_.get(Tile("c3"))) mustBe None
    }
  }

  "Black pawn" should {
    "be able to move 1 tile downward" in {
      Given("default initial arrangement of chess pieces")
      val board = Board(State.getInitState)
      board.isDefined mustBe true

      When("black pawn from tile d7 moves 1 tile downward")
      val blackPawn = board.flatMap(_.get(Tile("d7")))
      val oneTileDownward = Tile("d6")
      val relocatedBlackPawn = blackPawn.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, oneTileDownward).toOption) )
      relocatedBlackPawn.isDefined mustBe true

      Then("black pawn should be in tile d6")
      board.flatMap(_.get(Tile("d6"))) mustBe relocatedBlackPawn
    }

    "not be able to move upward" in {
      Given("arrangement where pawn from d7 is already in d6")
      val initState = State.getInitState
      initState.set(Pawn(4, Color.Black, Tile("d7")), Tile("d6")).isSuccess mustBe true

      val board = Board(initState)
      board.isDefined mustBe true

      When("black pawn from tile d6 moves 1 tile upward")
      val blackPawn = board.flatMap(_.get(Tile("d6")))
      blackPawn.isDefined mustBe true
      val oneTileUpward = Tile("d7")
      val relocatedBlackPawn = blackPawn.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, oneTileUpward).toOption) )

      Then("black pawn should still be in tile d6")
      relocatedBlackPawn mustBe None
      board.flatMap(_.get(Tile("d6"))) mustBe blackPawn
    }

    "be able to move 2 tiles downward as first move only" in {
      Given("default initial arrangement of chess pieces")
      val board = Board(State.getInitState)
      board.isDefined mustBe true

      When("black pawn from tile d7 moves two tiles downward")
      val blackPawn = board.flatMap(_.get(Tile("d7")))
      val twoTilesDownward = Tile("d5")
      val relocatedBlackPawn = blackPawn.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, twoTilesDownward).toOption) )
      relocatedBlackPawn.isDefined mustBe true

      Then("black pawn should be in tile d5")
      board.flatMap(_.get(Tile("d5"))) mustBe relocatedBlackPawn
    }

    "not be able to move two tiles downward after its first move" in {
      Given("default initial arrangement of chess pieces")
      val board = Board(State.getInitState)
      board.isDefined mustBe true

      When("black pawn from tile d7 moves one tile downward")
      val blackPawn = board.flatMap(_.get(Tile("d7")))
      val oneTileDownward = Tile("d6")
      val blackPawnInD6 = blackPawn.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, oneTileDownward).toOption) )
      blackPawnInD6.isDefined mustBe true

      When("black pawn, now in d6, moves two tiles downward")
      val twoTileDownward = Tile("d4")
      val blackPawnInD4 = blackPawnInD6.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, twoTileDownward).toOption))

      Then("black pawn should not be in tile d4")
      board.flatMap(_.get(Tile("d4"))) mustBe None
      blackPawnInD4 mustBe None
      Then("black pawn should still be in tile d6")
      board.flatMap(_.get(Tile("d6"))) mustBe blackPawnInD6
    }

    "be able to move 1 tile downward after its first move" in {
      Given("default initial arrangement of chess pieces")
      val board = Board(State.getInitState)
      board.isDefined mustBe true

      When("black pawn from tile d7 moves one tile upward")
      val blackPawn = board.flatMap(_.get(Tile("d7")))
      blackPawn.isDefined mustBe true
      val oneTileDownward = Tile("d6")
      val blackPawnInD6 = blackPawn.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, oneTileDownward).toOption) )
      blackPawnInD6.isDefined mustBe true

      When("black pawn, now in d36 moves one tile downward")
      val oneTileDownwardAgain = Tile("d5")
      val blackPawnInD5 = blackPawnInD6.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, oneTileDownwardAgain).toOption))
      blackPawnInD5.isDefined mustBe true

      Then("black pawn should no longer be in tile d6")
      board.flatMap(_.get(Tile("d6"))) mustBe None
      Then("black pawn should not be in tile d5")
      board.flatMap(_.get(Tile("d5"))) mustBe blackPawnInD5
    }

    "not be able to move upward if friendly is blocking the way" in {
      Given("friendly knight from b8 is occupying c6")
      val initState = State.getInitState
      initState.set(Knight(1, Color.Black, Tile("b8")), Tile("c6")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black pawn from tile c7 moves 1 tile downward")
      val blackPawn = board.flatMap(_.get(Tile("c7")))
      blackPawn mustBe Some(Pawn(3, Color.Black, Tile("c7")))
      val oneTileDownward = Tile("c6")
      val relocatedBlackPawn = blackPawn.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, oneTileDownward).toOption) )
      relocatedBlackPawn mustBe None

      Then("black pawn should still be in c7 because friendly knight is blocking")
      board.flatMap(_.get(Tile("c7"))) mustBe blackPawn
      board.flatMap(_.get(Tile("c6"))) mustBe Some(Knight(1, Color.Black, Tile("c6")))
    }

    "not be able to move jump downward 2 tiles if friendly is blocking the first tile" in {
      Given("friendly knight from b8 is occupying c6")
      val initState = State.getInitState
      initState.set(Knight(1, Color.Black, Tile("b8")), Tile("c6")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      When("black pawn from tile c7 moves 2 tiles downward")
      val blackPawn = board.flatMap(_.get(Tile("c7")))
      blackPawn mustBe Some(Pawn(3, Color.Black, Tile("c7")))
      val twoTilesDownward = Tile("c5")
      val relocatedBlackPawn = blackPawn.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, twoTilesDownward).toOption) )
      relocatedBlackPawn mustBe None

      Then("black pawn should still be in c7 because friendly knight is blocking")
      board.flatMap(_.get(Tile("c7"))) mustBe blackPawn
      board.flatMap(_.get(Tile("c6"))) mustBe Some(Knight(1, Color.Black, Tile("c6")))
    }

    "be able to move 1 tile diagonally downward when capturing" in {
      Given("2 white chess pieces located at tile c6 and f6")
      val initState = State.getInitState
      initState.set(Pawn(3, Color.White, Tile("c2")), Tile("c6")).isSuccess mustBe true
      initState.set(Pawn(6, Color.White, Tile("f2")), Tile("f6")).isSuccess mustBe true
      val board = Board(initState)
      board.isDefined mustBe true

      val whitePawnInC6 = board.flatMap(_.get(Tile("c6")))
      whitePawnInC6.isDefined mustBe true
      val whitePawnInF6 = board.flatMap(_.get(Tile("f6")))
      whitePawnInF6.isDefined mustBe true

      When("black pawn from tile d7 moves 1 tile diagonally left downward to tile c6")
      val blackPawnInD7 = board.flatMap(_.get(Tile("d7")))
      val oneTileLeftDiagonallyDownward = Tile("c6")
      val blackPawnInC6 = blackPawnInD7.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, oneTileLeftDiagonallyDownward).toOption) )
      blackPawnInC6.isDefined mustBe true

      Then("white pawn in c6 should be captured by black pawn originally from d7")
      board.flatMap(board=> whitePawnInC6.flatMap(board.get(_))) mustBe None
      board.flatMap(_.get(Tile("c6"))) mustBe blackPawnInC6

      When("black pawn from tile e7 moves 1 tile diagonally right downward to tile f6")
      val blackPawnInE7 = board.flatMap(_.get(Tile("e7")))
      val oneTileRightDiagonallyDownward = Tile("f6")
      val blackPawnInF6 = blackPawnInE7.flatMap(blackPawn=> board.flatMap(_.set(blackPawn, oneTileRightDiagonallyDownward).toOption) )
      blackPawnInF6.isDefined mustBe true

      Then("white pawn in f6 should be captured by black pawn originally from e7")
      board.flatMap(board=> whitePawnInF6.flatMap(board.get(_))) mustBe None
      board.flatMap(_.get(Tile("f6"))) mustBe blackPawnInF6
    }

    "not able to move diagonally when not capturing" in {
      Given("default initial arrangement of chess pieces")
      val board = Board(State.getInitState)
      board.isDefined mustBe true

      When("black pawn from tile d7 move diagonally to tile c6")
      val blackPawnInD7 = board.flatMap(_.get(Tile("d7")))
      val oneTileLeftDiagonallyDownward = Tile("c6")
      val result = blackPawnInD7.flatMap(blackPawn=> board.map(_.set(blackPawn, oneTileLeftDiagonallyDownward)) )
      result.map(_.isFailure) mustBe Some(true)

      Then("black pawn still remains in d7")
      board.flatMap(_.get(Tile("d7"))) mustBe blackPawnInD7
      board.flatMap(_.get(Tile("d6"))) mustBe None
    }
  }
}
