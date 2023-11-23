package tictactoe

import tictactoe.domain._

object MiniMax {

  /**
   * Function that performs the best move selection based on Minimax algorithm
   */
  def selectBestMove(givenState: State, depth: Int): Cell = {

    /**
     * A recursive functions that implements the Minimax algorithm.
     *
     * @param state       - current game state
     * @param targetMove  - a cell that represents a possible move
     * @param targetScore - a desired score that a player wants to achieve by applying a `targetMove`.
     *                    targetScore is 1 for a player who wants to maximise its result.
     *                    targetScore is -1 for a player who wants to minimise a result of an adversary.
     * @param depth       - a number of moves that should be evaluated ahead when calculating the best move
     * @return the best cell and the corresponding score that will be achieved if moving to this cell
     */
    def minimax(state: State, targetMove: Cell, targetScore: TargetScore, depth: Int): (Score, Cell) =
      state.gameResult match {
        // if the game result is the win of one of players or a draw - return the corresponding score and the target cell
        case GameResult.Win(winPlayerTag: PlayerTag) =>
          if (winPlayerTag == givenState.currentPlayer) (Score(1), targetMove)
          else (Score(-1), targetMove)

        case GameResult.Draw => (Score(0), targetMove)

        // if the game should go on:
        // - get all available moves
        // - iterate over all available moves using 'foldLeft' function, where as an initial result (accumulator)
        //     is a tuple of worst score (-targetScore) and targetMove.
        //     Then, one-by-one, we apply each available move to the game state ('state.applyMove') and recursively call 'minimax' function
        //     with the updated state and the new move. Also targetScore is reversed (since next move should be done by adversary) and
        //     depth is decremented by 1.
        //     The result score is multiplied by targetScore (1 or -1) and compared with accumulating bestScore.
        // - in the end of iteration we get the best possible move and corresponding score.
        case GameResult.Ongoing =>
          val (bestScore, bestMove) =
            state.availableMoves.map(_.cell).foldLeft((Score(-targetScore.value), targetMove)) { case ((bestScore, bestMove), move) =>
              if (bestScore.value == targetScore.value || depth == 0) (bestScore, bestMove)
              else {
                val updatedState = state.applyMove(move)
                val (score, _) = minimax(updatedState, move, targetScore.adversary, depth - 1)
                if (score.value * targetScore.value >= bestScore.value) (score, move)
                else (bestScore, bestMove)
              }
            }
          (bestScore, bestMove)
      }

    // the 1st iteration of minimax function.
    // we invoke it with a dummy Cell(-1, -1)
    val (_, bestMove) = minimax(givenState, Cell(-1, -1), TargetScore(1), depth)
    bestMove
  }

}
