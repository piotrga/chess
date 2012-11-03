package chess

import math._
import scala.Some

final case class Position(x: Int, y: Int) {
  def withinOneFieldOf(that: Position) = abs(x - that.x) <= 1 && abs(y - that.y) <= 1
  def onTheSameRowOrColumnAs(that: Position) = x == that.x || y == that.y
  def onDiagonalWith(that: Position) = abs(x - that.x) == abs(y - that.y)
}

trait Piece{
  def attacksField(piece: Position, field: Position) : Boolean
  def mkString : String
}

case class Board(M:Int, N:Int, pieces : Map[Position, Piece] = Map.empty){
  def fields : List[Position] = ((for {x <- 0 until M; y <- 0 until N} yield Position(x,y)).toSet -- pieces.keys.toSet).toList
  def tryPiece(newPiece:Piece, newPos:Position) : Option[Board] =
    if( canPutPieceAtPosition(newPiece, newPos) )
      Some(Board(M, N, pieces + (newPos -> newPiece)))
    else
      None


  private def canPutPieceAtPosition(newPiece:Piece, newPos:Position): Boolean = {
    pieces.forall {case (pos, piece) => !piece.attacksField(pos, newPos) && !newPiece.attacksField(newPos, pos)}
  }

  def mkString(rowSeparator:String = "\n") = {
    val stringRepresentation = for {y <- 0 until N; x <- 0 until M} yield pieces.get(Position(x, y)) map (_.mkString) getOrElse (".")
    stringRepresentation grouped M map (_.reduce(_+_)) mkString rowSeparator
  }
}


