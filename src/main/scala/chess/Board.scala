package chess

import math._
import scala.Some

final case class Position(x: Int, y: Int) {
  // These operations are carefully crafted to squeeze max performance as they are used in the core of the loop!

  def withinOneFieldOf(that: Position) = abs(x - that.x) <= 1 && abs(y - that.y) <= 1
  def onTheSameRowOrColumnAs(that: Position) = x == that.x || y == that.y
  def onDiagonalWith(that: Position) = abs(x - that.x) == abs(y - that.y)
}

trait Piece{
  def attacksField(piece: Position, field: Position) : Boolean
  def mkString : String
}

case class Board(M:Int, N:Int, pieces : Map[Position, Piece] = Map.empty){
  def fields : Stream[Position] = (for {x <- 0 until M; y <- 0 until N} yield Position(x,y)).toStream

  def tryPiece(newPiece:Piece, newPos:Position) : Option[Board] =
    if( canPutPieceAtPosition(newPiece, newPos) )
      Some(Board(M, N, pieces + (newPos -> newPiece)))
    else
      None

  private def canPutPieceAtPosition(newPiece:Piece, newPos:Position): Boolean = {
    pieces.forall {case (pos, piece) => !piece.attacksField(pos, newPos) && !newPiece.attacksField(newPos, pos)}
  }

  def mkString(rowSeparator:String = "\n") = {
    val b = Array.fill(M,N)(".") // I know it is mutable, but it makes it 2x faster, and still readable
    for ((pos, piece) <- pieces) b(pos.x)(pos.y) = piece.mkString
    b. map(_.mkString ) mkString rowSeparator
  }

}


