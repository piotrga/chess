package chess


object ConstantMemory_O_MxNtoK{

  def findSolutions(board: Board, pieces: Map[Piece,Int], listener: SolutionsListener) = {
    require( pieces.values forall( _ > 0), "Number of pieces must be > 0" )
    val stopParallelism = pieces.size - 2


    def findSolutions(board: Board, pieces: MSet[Piece], fields: List[Position]){
      if (pieces.isEmpty)
        listener.found(board)
      else{
        fields match{
          case pos :: remainingFields =>
            val piecesToIterate = if (pieces.size < stopParallelism ) pieces.keys else pieces.keys.par
            for { piece <- piecesToIterate} {
              (board tryPiece (piece, pos))
                .map (newValidBoard => findSolutions(newValidBoard, pieces - piece, remainingFields))
            }
            findSolutions(board, pieces, remainingFields)
          case Nil => ()
        }
      }
    }

    findSolutions(board, MSet(pieces), board.fields)
  }

  final case class MSet[K](private val map: Map[K,Int]) {
    def size = map.size

    def isEmpty = map.isEmpty
    def keys = map.keys
    def -(key : K) : MSet[K] = map.get(key) match {
      case None => this
      case Some(1) => MSet(map - key)
      case Some(i) => MSet(map + (key -> (i - 1)))
    }
  }

}

