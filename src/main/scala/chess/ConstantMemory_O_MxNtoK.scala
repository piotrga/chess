package chess

object ConstantMemory_O_MxNtoK{

  def findSolutions(board: Board, pieces: Map[Chess,Int]) : Solutions = {
    require( pieces.values forall( _ > 0), "Number of pieces must be > 0" )

    val solutions = new Solutions

    def findSolutions(board: Board, pieces: MSet[Chess], fields: List[(Int,Int)]){
      if (pieces.isEmpty)
        solutions.found(board)
      else{
        fields match{
          case pos :: remainingFields =>
            for { piece <- pieces.keys.par} {
              (board withPiece (piece, pos))
                .map (newBoard => findSolutions(newBoard, pieces - piece, remainingFields))
            }
            findSolutions(board, pieces, remainingFields)
          case Nil => ()
        }

      }
    }

    findSolutions(board, MSet(pieces), board.fields)
    solutions
  }

  case class MSet[K](private val map: Map[K,Int]) {
    def isEmpty = map.isEmpty
    def keys = map.keys
    def -(key : K) : MSet[K] = map.get(key) match {
      case None => this
      case Some(1) => MSet(map - key)
      case Some(i) => MSet(map + (key -> (i - 1)))
    }
  }

}
