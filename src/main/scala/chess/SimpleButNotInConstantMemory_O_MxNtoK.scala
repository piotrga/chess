package chess

object SimpleButNotInConstantMemory_O_MxNtoK{

  class DuplicateCheckingSolutions(delegate: SolutionsListener){
    private var solutions = Set[Board]()

    def found(board: Board) = synchronized {
      if (!solutions.contains(board)){
        solutions += board
        delegate.found(board)
      }
    }
  }

  def findSolutions(board: Board, pieces: Iterable[Piece], listener: SolutionsListener)  = {
    val solutions = new DuplicateCheckingSolutions(listener)

    def findSolutions(board: Board, pieces: Iterable[Piece]){
      pieces match{
        case piece :: remainingPieces =>
          for { pos <- board.fields.par} {
            (board tryPiece (piece, pos))
              .map (newBoard => findSolutions(newBoard, remainingPieces))
          }
        case Nil => solutions.found(board)

      }
    }

    findSolutions(board, pieces)
    solutions
  }

}
