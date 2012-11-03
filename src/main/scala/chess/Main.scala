package chess

import utils._

object Main extends App{
  lazy val findSolutions = ConstantMemory_O_MxNtoK.findSolutions _

  override def main(args: Array[String]) {
    solve(Board(3, 3), Map(King -> 2, Rook -> 1))
    solve(Board(4, 4 ), Map( Knight -> 4, Rook -> 2 ) )

    solve(Board(6, 9 ), Map(King -> 2, Queen -> 1, Bishop -> 1, Rook -> 1, Knight -> 1 ), new FastSolutionsListener )
  }

  def solve(board: Board, pieces: Map[Piece, Int], listener : SolutionsListener = new SysOutSolutionsListener) {
    val (duration, _) = time(findSolutions(board, pieces, listener))
    println("Found %d solutions in %dms" format(listener.count, duration))
  }
}

