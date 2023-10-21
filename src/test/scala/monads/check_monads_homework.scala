package monads
import monad._
import org.scalatest.flatspec.AnyFlatSpec

class check_monads_homework  extends AnyFlatSpec  {


  def mult20(x: Int): Wrap[Int] = NonEmptyWrap(x * 20)
  def plus6(x: Int): Wrap[Int] = NonEmptyWrap(x + 6)

  //left unit lww
  assert(NonEmptyWrap(3).flatMap(mult20) == mult20(3))
  println("leftUnitLaw check success")


  //right unit law
  private val monad: Wrap[Int] = NonEmptyWrap(3)
  assert(monad.flatMap(x => NonEmptyWrap(x) ) == monad )
  println("rightUnitLaw check success")


  //associative law
  private val left = monad.flatMap(mult20).flatMap(plus6)
  private val right = monad.flatMap(x => mult20(x).flatMap(plus6))
  assert(left == right)
  println("associativityLaw check success")

  assert(Wrap.empty[Int] == EmptyWrap)
  println("empty check success")

  assert(NonEmptyWrap(25).get == 25)
  println("get check success")

  assert(NonEmptyWrap((26, 25)).flatMap(x => if (x._1 - x._2 >= 0) NonEmptyWrap(x._1 - x._2) else EmptyWrap) == NonEmptyWrap(1))
  assert(NonEmptyWrap((25, 26)).flatMap(x => if (x._1 - x._2 >= 0) NonEmptyWrap(x._1 - x._2) else EmptyWrap) == EmptyWrap)
  println("flatMap check success")

  assert(NonEmptyWrap((26, 25)).map(x => x._1 + x._2) == NonEmptyWrap(51))
  assert(EmptyWrap.map( x => 2 == 2) == EmptyWrap)
  println("map check success")

  assert(NonEmptyWrap((26, 25)).map_2(x => x._1 + x._2) == NonEmptyWrap(51))
  assert(EmptyWrap.map_2(x => 2 == 2) == EmptyWrap)
  println("map_2 check success")

  def isEven(number: Int) = number % 2 == 0
  assert(NonEmptyWrap(10).withFilter(x => isEven(x)) == NonEmptyWrap(10))
  assert(NonEmptyWrap(7).withFilter(x => isEven(x)) == EmptyWrap)
  println("withFilter check success")

}
