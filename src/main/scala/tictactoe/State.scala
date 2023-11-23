package tictactoe

import tictactoe.domain._

/**
 * Represents a game state.
 * The game state consists of a game board (GameBoard) and the tag for a current player who should move next (PlayerTag)
 */
case class State(board: GameBoard, currentPlayer: PlayerTag) {

  private val noEmptyCells: Boolean = !board.cells.exists(_.playerTag == PlayerTag.Empty)
  private val xPlayerWon: Boolean = checkPlayerWin(PlayerTag.X)
  private val oPlayerWon: Boolean = checkPlayerWin(PlayerTag.O)
  private val gameOver: Boolean =
    noEmptyCells || xPlayerWon || oPlayerWon

  private def checkPlayerWin(playerTag: PlayerTag): Boolean =
    board.rows.exists(lineFilledByPlayer(_, playerTag)) ||
      board.columns.exists(lineFilledByPlayer(_, playerTag)) ||
      board.diagonals.exists(lineFilledByPlayer(_, playerTag))

  private def lineFilledByPlayer(line: Array[CellWithTag], playerTag: PlayerTag): Boolean =
    line.forall(_.playerTag == playerTag)


  // Returns the game result from a current state
  val gameResult: GameResult = {
    if (gameOver) {
      if (xPlayerWon) GameResult.Win(PlayerTag.X)
      else if (oPlayerWon) GameResult.Win(PlayerTag.O)
      else GameResult.Draw
    } else GameResult.Ongoing
  }

  // Returns available cells to move
  val availableMoves: Array[CellWithTag] = board.cells.filter(_.playerTag == PlayerTag.Empty)


  /**
   * Applies a move to the current state. This function marks a particular cell with a player tag
   * who should move next (currentPlayer from the state).
   * Returns an updated state.
   */
  def applyMove(cell: Cell): State = {
    val updatedCells =
      board.cells.toBuffer
        .updated((cell.row - 1) * board.dimension + (cell.column - 1), CellWithTag(cell, currentPlayer)).toArray

    val updatedBoard = GameBoard(updatedCells)
    State(updatedBoard, currentPlayer.adversary)
  }
}
