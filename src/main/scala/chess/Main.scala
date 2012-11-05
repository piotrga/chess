package chess

import utils._

object Main extends App{
  lazy val findSolutions = DFS_MxN_memory_O_MxNtoK.findSolutions _

  override def main(args: Array[String]) {
    solve(Board(3, 3), pieces = Map(King -> 2, Rook -> 1))
    solve(Board(4, 4 ), pieces = Map( Knight -> 4, Rook -> 2 ) )

    solve(Board(4, 4 ), pieces = Map(Queen -> 3))
//    solve(Board(6, 9 ), pieces = Map(King -> 2, Queen -> 1, Bishop -> 1, Rook -> 1, Knight -> 1 ))
  }

  def solve(board: Board, pieces: Map[Piece, Int], listener : SolutionsListener = new SysOutSolutionsListener) {
    val (duration, _) = time{
      findSolutions(board, pieces, listener)
      listener.done()
    }

    System.err.println("Found %d solutions in %dms" format(listener.count, duration))
  }
}

