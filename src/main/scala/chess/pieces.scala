package chess

case object King extends Piece{
  def attacksField(myPos: Position, field:Position) = myPos withinOneFieldOf field
  def mkString = "K"
}

case object Rook extends Piece{
def attacksField(myPos: Position, field: Position) = myPos onTheSameRowOrColumnAs field
  def mkString = "R"
}

case object Bishop extends Piece{
  def attacksField(myPos: Position, field: Position) =  myPos onDiagonalWith field
  def mkString = "B"
}

case object Queen extends Piece{
  def attacksField(myPos: Position, field: Position) = (myPos onDiagonalWith field) || ( myPos onTheSameRowOrColumnAs field)
  def mkString = "Q"
}

case object Knight extends Piece{
  import math._

  def attacksField(myPos: Position, field: Position) = abs((myPos.x - field.x) * (myPos.y - field.y)) == 2
  def mkString = "N"
}
