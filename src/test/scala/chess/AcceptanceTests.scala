package chess

import org.scalatest.FreeSpec
import org.scalatest.matchers.MustMatchers

class AcceptanceTests extends FreeSpec with MustMatchers{
  val findSolutions = DFS_MxN_memory_O_MxNtoK.findSolutions _

  val s1 =
    """.R.
      |...
      |K.K""".stripMargin

  val s2 =
    """K.K
      |...
      |.R.""".stripMargin

  val s3 =
    """..K
      |R..
      |..K""".stripMargin

  val s4 =
    """K..
      |..R
      |K..""".stripMargin

  "3x3, 2 Kings, 1 Rook" in {
    val store = new SolutionsStore
    findSolutions(Board(3, 3), Map(King -> 2, Rook -> 1), store)
    store.solutions.map(_.mkString()).toSet must be (Set(s1,s2,s3,s4))
  }

  "4x4, 2 Knights, 2 Rooks " in {
    val store = new SolutionsStore
    findSolutions(Board(4, 4 ), Map( Knight -> 4, Rook -> 2 ), store)
    store.count must be (8)
    store.solutions.map(_.mkString()) must contain(""".K.K
                                                     |R...
                                                     |.K.K
                                                     |..R.""".stripMargin)
  }

}
