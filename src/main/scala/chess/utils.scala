package chess

object utils {

  def time[T](f: => T) : (Long, T) ={
    val start = System.currentTimeMillis()
    val res : T = f
    (System.currentTimeMillis() - start, res)
  }

}
