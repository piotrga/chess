package chess

import java.util.concurrent.atomic.AtomicInteger

object Chess {

  implicit def toRichPos(pos :(Int,Int)) = new {
    def isNextTo(that:(Int,Int))  = pos!=that && (pos._1 - that._1).abs <= 1 && (pos._2 - that._2).abs <= 1
    def onTheSameRowOrColumnAs(that:(Int,Int)) = pos._1 == that._1 || pos._2 == that._2
    def onDiagonalWith(that:(Int,Int)) = (pos._1 - that._1).abs == (pos._2 - that._2).abs
  }

  def time[T](f: => T) : (Long, T) ={
    val start = System.currentTimeMillis()
    val res : T = f
    (System.currentTimeMillis() - start, res)
  }

}

import Chess._

case class Board(M:Int, N:Int, pieces : Map[(Int,Int), Chess] = Map.empty){
  def fields : List[(Int,Int)] = ((for {x <- 0 until M; y <- 0 until N} yield(x,y)).toSet -- pieces.keys.toSet).toList
  def withPiece(newPiece:Chess, newPos:(Int, Int)) : Option[Board] =
    if( canPutPieceAtPosition(newPiece, newPos) )
      Some(Board(M, N, pieces + (newPos -> newPiece)))
    else
      None


  private def canPutPieceAtPosition(newPiece:Chess, newPos:(Int, Int)): Boolean = {
    pieces.forall {case (pos, piece) => !piece.attacksField(pos, newPos) && !newPiece.attacksField(newPos, pos)}
  }

  def mkString = {
    val stringRepresentation = for {y <- 0 until N; x <- 0 until M} yield pieces.get(x, y) map (_.mkString) getOrElse (".")
    val boardSquare = stringRepresentation.grouped(M).map("|"+_.reduce(_+_)+"|").mkString("\n\t")
    "Board[%dx%d]:\n\t%s" format (M,N, boardSquare)
  }
}

class Solutions{
  private val solutionsFound = new AtomicInteger(0)
  def count : Int = solutionsFound.get()

  def found(board: Board)  {
    solutionsFound.incrementAndGet()
    println(board.mkString)
  }
}


sealed trait Chess{
  def attacksField(pos: (Int,Int), field:(Int,Int)) : Boolean
  def mkString : String
}


case object King extends Chess{
  def attacksField(myPos: (Int, Int), field:(Int,Int)) = myPos isNextTo field
  def mkString = "K"
}

case object Rook extends Chess{ //wierza
def attacksField(myPos: (Int, Int), field: (Int, Int)) = myPos onTheSameRowOrColumnAs field
  def mkString = "R"
}

case object Bishop extends Chess{
  def attacksField(myPos: (Int, Int), field: (Int, Int)) = myPos onDiagonalWith field
  def mkString = "B"
}

case object Queen extends Chess{
  def attacksField(myPos: (Int, Int), field: (Int, Int)) = (myPos onDiagonalWith field) || (myPos onTheSameRowOrColumnAs field)
  def mkString = "Q"
}

case object Knight extends Chess{
  def attacksField(myPos: (Int, Int), field: (Int, Int)) = {
    val delta = ((myPos._1 - field._1).abs, (myPos._2 - field._2).abs)
    delta == (1,2) || delta == (2,1)
  }

  def mkString = "K"
}
