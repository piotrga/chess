package chess

import java.util.concurrent.atomic.AtomicInteger

class SolutionsListener{
  private val solutionsFound = new AtomicInteger(0)
  def count : Int = solutionsFound.get()

  def found(board: Board)  {
    solutionsFound.incrementAndGet()
  }
}

class SysOutSolutionsListener extends SolutionsListener{
  override def found(board: Board)  {
    super.found(board)
    println("Board[%dx%d]:\n\t|%s|\n" format (board.M, board.N, board.mkString("|\n\t|")))
  }
}

class SolutionsStore extends SolutionsListener{
  private var _solutions = List[Board]()
  def solutions = _solutions.toList
  override def found(board: Board) = synchronized{
    super.found(board)
    _solutions +:= board
  }
}

class FastSolutionsListener extends SolutionsListener{
  override def found(board: Board)  {
    super.found(board)
    if(count % 10000 == 0) println(count/1000+"k")
  }
}
