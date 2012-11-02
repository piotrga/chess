package chess

import Chess._

object Main2 extends App{


  //  val solutions1 = Alg1.findSolutions(Board(9, 9 ), List(King, King, Wierza))
  //  println("[Alg1] Found %d solutions in %dms" format(solutions1.count, solutions1.duration))

  override def main(args: Array[String]) {
    //  val solutions2 = Alg2.findSolutions(Board(6, 9 ), Map(King -> 2, Queen -> 1, Goniec -> 1, Skoczek -> 1, Wierza -> 1 ) )
    val (duration2, solutions2) = time (ConstantMemory_O_MxNtoK.findSolutions(Board(3, 3 ), Map( King -> 2, Rook -> 1 ) ))
    println("[Alg2] Found %d solutions in %dms" format(solutions2.count, duration2))

    val (duration3, solutions3) = time (ConstantMemory_O_MxNtoK.findSolutions(Board(4, 4 ), Map( Knight -> 4, Rook -> 2 ) ))
    println("[Alg2] Found %d solutions in %dms" format(solutions3.count, duration3))

    //  a 6x9 board with 2 Kings, 1 Queen, 1 Bishop, 1 Rook and 1 Knight.
    val (duration4, solutions4) = time (ConstantMemory_O_MxNtoK.findSolutions(Board(6, 9 ), Map(King -> 2, Queen -> 1, Bishop -> 1, Rook -> 1, Knight -> 1 ) ))
    println("[Alg2] Found %d solutions in %dms" format(solutions3.count, duration3))

  }
}
