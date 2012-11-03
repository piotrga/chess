package chess

object Alg1{
  class DuplicateCheckingSolutions extends SolutionsListener{
    private var solutions = Set[Board]()

    override def count = solutions.size
    override def found(board: Board)= synchronized{
      if (!solutions.contains(board)){
        solutions += board
        //          println(board.mkString)
      }
    }
  }

  def findSolutions(board: Board, pieces: Iterable[Piece]) : SolutionsListener = {
    val solutions = new DuplicateCheckingSolutions
    findSolutions(board, pieces, solutions)
    solutions
  }

  def findSolutions(board: Board, pieces: Iterable[Piece], solutions:SolutionsListener){
    pieces match{
      case piece :: remainingPieces =>
        for { pos <- board.fields.par} {
          (board tryPiece (piece, pos))
            .map (newBoard => findSolutions(newBoard, remainingPieces, solutions))
        }
      case Nil => solutions.found(board)

    }
  }
}
