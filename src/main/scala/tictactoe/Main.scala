package tictactoe

import tictactoe.domain._

import scala.annotation.tailrec


object Main {

  /**
   * Main function to launch the program
   */
  def main(args: Array[String]): Unit = {
    // to keep it simple we assume the dimension of the game board is 3x3
    val dimension = 3

    // this is a number of moves the computer looks ahead when calculating the best move using MiniMax algorithm
    // max number of moves on the 3x3 board is 9
    val depth = 9

    // get the tag for human player: x or o
    val humanTag = IO.getHumanTagInput

    // initialize the game state
    // player X always starts first
    val state = State(GameBoard.empty(dimension), PlayerTag.X)

    println(state.board.toString)
    play(state, humanTag, depth)
  }


  /**
   * A recursive function that runs the game until the over (the win of one of players or a draw)
   */
  @tailrec
  private def play(state: State, human: HumanTag, depth: Int): Unit = {
    // Before each move we check the game state and determine whether there is a game result (win or draw)
    // or the game should go on (no winner yet and there are empty cells on the board)
    state.gameResult match {
      case GameResult.Draw =>
        println("\n\n\n========= Game Over =========")
        println(state.board.toString)
        println()
        print("Draw!")

      case GameResult.Win(p) =>
        println("\n\n\n========= Game Over =========")
        println(state.board.toString)
        println()
        if (p == human.playerTag) print("Congrats! You won the game!")
        else print("Sorry, you lost :(")

      case GameResult.Ongoing =>
        val selectedCell: Cell =
          if (state.currentPlayer == human.playerTag) {
            // if a human's turn to move - ask to select a cell
            humanMove(state)
          }
          else {
            // if a computer's turn to move - determine the best cell to move using the MiniMax algorithm
            aiMove(state, depth)
          }

        // update the game state by applying the selected cell
        val updatedState = state.applyMove(selectedCell)
        println(updatedState.board.toString)

        // recursively call the "play()" function with the updated state
        play(updatedState, human, depth)
    }
  }

  private def aiMove(state: State, depth: Int): Cell = {
    println("AI makes a decision...")
    MiniMax.selectBestMove(state, depth)
  }

  @tailrec
  private def humanMove(state: State): Cell = {
    // get empty cells from the game state that are available to move to
    val availableCells = state.availableMoves.map(_.cell)

    // get human's input for a row and a column numbers to determine a cell to move to
    val row = IO.getNumberInput(state, "row")
    val column = IO.getNumberInput(state, "column")
    val humanCell = Cell(row, column)

    // verify that the human selected an empty cell
    if (!availableCells.contains(humanCell)) {
      println(s"The cell $row:$column is not available, try again")
      humanMove(state)
    } else humanCell
  }

}
