import scala.annotation.tailrec


def fibonachi(n: Long): List[Long] = {
  @tailrec
  def inner(cur: Long, prev: Long, sumFib: (Long, List[Long])): (Long, List[Long]) = {
    cur match {
      case 0 => (prev, sumFib._2)
      case _ => inner(cur - 1, sumFib._1, (prev + sumFib._1, prev +: sumFib._2))
    }

  }
  val fibC = inner(n, 0, (1, List()))
  (fibC._1 +: fibC._2).reverse
}

fibonachi(0)
fibonachi(1)
fibonachi(2)
fibonachi(3)
fibonachi(4)
fibonachi(5)
fibonachi(6)
fibonachi(7)
fibonachi(8)    
fibonachi(9)
