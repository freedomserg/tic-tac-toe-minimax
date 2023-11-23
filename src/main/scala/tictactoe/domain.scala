package tictactoe

/**
 * Domain entities that represent the tic-tac-toe game
 */
object domain {

  trait GameResult

  object GameResult {
    case class Win(playerTag: PlayerTag) extends GameResult
    case object Draw extends GameResult
    case object Ongoing extends GameResult
  }

  trait PlayerTag {
    def adversary: PlayerTag
    def str: String
  }

  object PlayerTag {
    case object X extends PlayerTag {
      override val adversary: PlayerTag = PlayerTag.O
      override val str: String = "x"
    }

    case object O extends PlayerTag {
      override val adversary: PlayerTag = PlayerTag.X
      override val str: String = "o"
    }

    case object Empty extends PlayerTag {
      override def adversary: PlayerTag = throw new RuntimeException("PlayerTag.Empty doesn't have an adversary")

      override val str: String = " "
    }
  }

  case class HumanTag(playerTag: PlayerTag)

  case class Cell(row: Int, column: Int)

  case class CellWithTag(cell: Cell, playerTag: PlayerTag)

  case class TargetScore(value: Int) {
    def adversary: TargetScore = TargetScore(-value)
  }

  case class Score(value: Int)

}
