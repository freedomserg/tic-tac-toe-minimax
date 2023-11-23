package tictactoe

import tictactoe.domain.{HumanTag, PlayerTag}

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object IO {

  /**
   * Utility function to get the human input for the tag (x or o)
   */
  @tailrec
  def getHumanTagInput: HumanTag = {
    print("Please choose X(x) or O(o): ")
    readLine() match {
      case s if s.toLowerCase == "x" => HumanTag(PlayerTag.X)
      case s if s.toLowerCase == "o" || s == "0" => HumanTag(PlayerTag.O)
      case c =>
        println(s"Invalid input: $c, try again")
        getHumanTagInput
    }
  }

  /**
   * Utility function to get the human input for row and column numbers
   */
  @tailrec
  def getNumberInput(state: State, inputName: String): Int = {
    val dimension = state.board.dimension
    print(s"Please enter $inputName: ")
    val input = readLine()
    Try(input.toInt) match {
      case Success(value) =>
        if (value < 0 || value > dimension) {
          println(s"Invalid input: $value, try again")
          getNumberInput(state, inputName)
        } else value
      case Failure(_) =>
        println(s"Invalid input: $input, try again")
        getNumberInput(state, inputName)
    }
  }

}
