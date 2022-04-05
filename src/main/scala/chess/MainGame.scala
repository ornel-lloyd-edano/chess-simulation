package chess

import chess.domain.{Board, Turn}
import chess.domain.Board.{State}
import chess.view.AsciiConsoleRender
import com.whitehatgaming.UserInputFile
import util.Converters.ArrayIntExtension

import scala.util.{Failure, Success, Try}

object MainGame extends App {
  val file = "src/main/resources/data/sample-moves.txt" //sample-moves-invalid.txt   //checkmate.txt
  Try(new UserInputFile(file)) match {
    case Success(userInputFile)=>
      var turns: Seq[Turn] = Nil
      var input: Array[Int] = userInputFile.nextMove()
      while( input != null) {
        Option(input).map(_.asTurn).foreach {
          case Success(turn)=>
            turns = turns :+ turn
            input = userInputFile.nextMove()
          case Failure(exception)=>
            println(s"A line of user input from the file is unreadable. Reason: [${exception.getMessage}]")
            System.exit(0)
        }
      }
      val state = State.getInitState
      Board(state) match {
        case Some(board)=>
          val renderer = new AsciiConsoleRender(state)
          turns.foreach {
            case Turn(src, dest)=>
              board.get(src).map { chessPiece =>
                board.set(chessPiece, dest)
              } match {
                case Some(Success(chessPiece))=>
                  renderer.renderBoard()
                  println(s"$chessPiece was moved from $src to $dest")
                case Some(Failure(exception))=>
                  println(s"Fail to move chess piece on tile $dest. Reason: [${exception.getMessage}]")
                  System.exit(0)
                case None=>
                  println(s"No chess piece was selected on tile $src")
                  System.exit(0)
              }
          }
        case None=>
          println("Invalid board state")
          System.exit(0)
      }

    case Failure(_)=>
      println(s"$file was not found")
      System.exit(0)
  }
}
