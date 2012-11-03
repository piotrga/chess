package chess

import collection.immutable.Stream.Empty

// This algorithm solves the problem in O( MxN ! / (MxN - K) !) ~= O(MxN ^ K) where K is a number of pieces.
// It is in the pessimistic case where all the pieces are unique.

// It does it in MxN stack frames so the memory consumption is MxN
object DFS_MxN_memory_O_MxNtoK{

  def findSolutions(board: Board, pieces: Map[Piece,Int], listener: SolutionsListener) = {
    require( pieces.values forall( _ > 0), "Number of pieces must be > 0" )
    val stopParallelism = pieces.size - 2


    // This method is not tail recursive, which means that solution will run out of stack if MxN is big
    // I could flatten it, and implement it without recursion, on my own stack or queue, but I decided not to do it
    // as the excercise guidelines clearly suggested that simple readable solution is preferred.
    def findSolutions(board: Board, remainingPieces: MSet[Piece], fields: Stream[Position]){
      if (remainingPieces.isEmpty)
        listener.found(board)
      else{
        fields match {
          case pos #:: remainingFields =>
            for { piece <- iterate(remainingPieces)} {
              (board tryPiece (piece, pos))
                .map (newValidBoard => findSolutions(newValidBoard, remainingPieces - piece, remainingFields))
            }
            findSolutions(board, remainingPieces, remainingFields)
          case Empty => ()
        }
      }
    }

    def iterate(remainingPieces: MSet[Piece]) = if (remainingPieces.size < stopParallelism) // this is rather silly criterion but it does the job
      remainingPieces.uniqueElements else remainingPieces.uniqueElements.par

    findSolutions(board, MSet(pieces), board.fields)
  }

  object MSet{
    def apply[K](map: Map[K,Int]) : MSet[K] = {
      MSet(map, size = map.values.sum)
    }
  }

  final case class MSet[K](private val map: Map[K,Int], size : Int) {
    def isEmpty = map.isEmpty
    def uniqueElements = map.keys

    def -(key : K) : MSet[K] = map.get(key) match {
      case None => this
      case Some(1) => MSet(map - key, size - 1)
      case Some(i) => MSet(map + (key -> (i - 1)), size - 1)
    }
  }

}

