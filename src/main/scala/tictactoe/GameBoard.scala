package tictactoe

import tictactoe.domain._

/**
 * Represents a game board. It contains an array of cells (CellWithTag).
 * The length of the array is dimension x dimension (for example, if dimension = 3 then array length is 9 )
 */
case class GameBoard(cells: Array[CellWithTag]) {

  val dimension: Int = Math.sqrt(cells.length).toInt

  // all rows of the board
  // a single row is represented by Array[CellWithTag], so all rows - Array[Array[CellWithTag]]
  val rows: Array[Array[CellWithTag]] =
    (for {
      row <- 0 until dimension
    } yield (for {
      column <- 0 until dimension
    } yield cells(row * dimension + column)).toArray).toArray

  // all columns of the board
  val columns: Array[Array[CellWithTag]] =
    (for {
      column <- 0 until dimension
    } yield (for {
      row <- 0 until dimension
    } yield cells(row * dimension + column)).toArray).toArray

  private val topLeftToBottomRightDiagonal: Array[CellWithTag] =
    (for {
      row <- 0 until dimension
      column = row
    } yield cells(row * dimension + column)).toArray

  private val bottomLeftToTopRightDiagonal: Array[CellWithTag] =
    (for {
      column <- 0 until dimension
      row = dimension - column - 1
    } yield cells(row * dimension + column)).toArray

  // all diagonals of the board
  val diagonals: Array[Array[CellWithTag]] = Array(topLeftToBottomRightDiagonal, bottomLeftToTopRightDiagonal)

  // string representation of the board, used to visualize in the console
  override def toString: String = {
    val rowSeparator = "-" * (dimension * 3 + dimension - 1)
    rows.map(_.map(cell => s" ${cell.playerTag.str} ").mkString("|"))
      .mkString(s"\n$rowSeparator\n")
  }
}

object GameBoard {
  // initialisation of an empty board (all cells contain empty tag)
  def empty(dimension: Int): GameBoard = {
    val cells = for {
      row <- 1 to dimension
      column <- 1 to dimension
    } yield CellWithTag(Cell(row, column), PlayerTag.Empty)
    GameBoard(cells.toArray)
  }
}
