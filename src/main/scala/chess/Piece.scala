package chess

import java.util.concurrent.atomic.AtomicInteger
import math._

object Piece {

  def isWithinOneField(pos :(Int,Int), that:(Int,Int))  = abs(pos._1 - that._1) <= 1 &&  abs(pos._2 - that._2) <=1
  def onTheSameRowOrColumnAs(pos :(Int,Int), that:(Int,Int)) = pos._1 == that._1 || pos._2 == that._2
  def onDiagonalWith(pos :(Int,Int), that:(Int,Int)) = abs(pos._1 - that._1) == abs(pos._2 - that._2)

  final case class Position(x: Int, y: Int) {
    def isWithinOneField(that: Position) = abs(x - that.x) <= 1 && abs(y - that.y) <= 1
    def onTheSameRowOrColumnAs(that: Position) = x == that.x || y == that.y
    def onDiagonalWith(that: Position) = abs(x - that.x) == abs(y - that.y)
  }

  def time[T](f: => T) : (Long, T) ={
    val start = System.currentTimeMillis()
    val res : T = f
    (System.currentTimeMillis() - start, res)
  }

}

import Piece._

case class Board(M:Int, N:Int, pieces : Map[(Int,Int), Piece] = Map.empty){
  def fields : List[(Int,Int)] = ((for {x <- 0 until M; y <- 0 until N} yield(x,y)).toSet -- pieces.keys.toSet).toList
  def tryPiece(newPiece:Piece, newPos:(Int, Int)) : Option[Board] =
    if( canPutPieceAtPosition(newPiece, newPos) )
      Some(Board(M, N, pieces + (newPos -> newPiece)))
    else
      None


  private def canPutPieceAtPosition(newPiece:Piece, newPos:(Int, Int)): Boolean = {
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
    val count = solutionsFound.incrementAndGet()
    if(count % 10000 == 0) println(count/1000+"k")
    //    println(board.mkString)
  }
}


sealed trait Piece{
  def attacksField(pos: (Int,Int), field:(Int,Int)) : Boolean
  def mkString : String
}


case object King extends Piece{
  //  def attacksField(myPos: (Int, Int), field:(Int,Int)) = myPos isWithinOneField field
  def attacksField(myPos: (Int, Int), field:(Int,Int)) =  isWithinOneField(myPos, field)
  def mkString = "K"
}

case object Rook extends Piece{ //wierza
//def attacksField(myPos: (Int, Int), field: (Int, Int)) = myPos onTheSameRowOrColumnAs field
def attacksField(myPos: (Int, Int), field: (Int, Int)) = onTheSameRowOrColumnAs(myPos, field)
  def mkString = "R"
}

case object Bishop extends Piece{
  def attacksField(myPos: (Int, Int), field: (Int, Int)) =  onDiagonalWith(myPos, field)
  def mkString = "B"
}

case object Queen extends Piece{
  def attacksField(myPos: (Int, Int), field: (Int, Int)) = (onDiagonalWith(myPos, field)) || ( onTheSameRowOrColumnAs(myPos, field))
  def mkString = "Q"
}

case object Knight extends Piece{
  def attacksField(myPos: (Int, Int), field: (Int, Int)) = abs((myPos._1 - field._1) * (myPos._2 - field._2)) == 2
  def mkString = "K"
}
