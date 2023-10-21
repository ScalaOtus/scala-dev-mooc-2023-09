def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

List("Lorem", "ipsum", "dolor", "sit", "amet").zipWithIndex.collect{
  case (first, 0) => first
  case (other , _) => if (isASCIIString(other)) other.toUpperCase else other.toLowerCase
}

List("Оказывается", ",", "ЗвУк", "КЛАВИШЬ").zipWithIndex.collect{
  case (first, 0) => first
  case (other , _) => if (isASCIIString(other)) other.toUpperCase else other.toLowerCase
}
val convertMap  = Map("0" -> "one" , "1" -> "one", "2" -> "two", "3" -> "three", "4" -> "four",
"5" -> "five", "6" -> "six", "7" -> "seven", "8" -> "eight", "9" -> "nine")
val regex = "[0-9]".r



regex.replaceAllIn("123 Main Street", "x")
regex.replaceAllIn("123 Main Street", s => convertMap.get(s.toString).getOrElse(s.toString) )



//////////////////////////////////////////////////////
object mnd {

  trait Wrap[+A] {

    def get: A

    def pure[R](x: R): Wrap[R] = NonEmptyWrap(x)

    def flatMap[R](f: A => Wrap[R]): Wrap[R] = this match {
      case EmptyWrap => EmptyWrap
      case NonEmptyWrap(a) => f(a)
    }


    // HINT: map можно реализовать через pure и flatMap
    def map[R](f: A => R): Wrap[R] = this match {
      case EmptyWrap => EmptyWrap
      case NonEmptyWrap(a) => pure(f(a))
    }

    //реализация через flatMap и  pure
    def map_2[R](f: A => R): Wrap[R] = flatMap(a => pure(f(a)))


    def withFilter(f: A => Boolean): Wrap[A] = flatMap(a => if (f(a)) this else EmptyWrap)

  }

  object Wrap {
    def empty[R]: Wrap[R] = EmptyWrap
  }

  case class NonEmptyWrap[A](result: A) extends Wrap[A] {
    override def get: A = result
  } // pure

  case object EmptyWrap extends Wrap[Nothing] {
    override def get: Nothing = throw new NoSuchElementException("Wrap.get")
  } // bottom, null element



}

import mnd._

NonEmptyWrap(1).get
NonEmptyWrap(2).get
NonEmptyWrap(2).map(_ * 2)
NonEmptyWrap(1).map(_ * 10)

NonEmptyWrap(2).map("=" + _.toString )
EmptyWrap.map(_ => "efeffr44f4f44bf")

NonEmptyWrap(2).flatMap(x => if (x >= 0) NonEmptyWrap(x * 5) else EmptyWrap)
NonEmptyWrap(-2).flatMap(x => if (x >= 0) NonEmptyWrap(x * 5) else EmptyWrap)

EmptyWrap.flatMap(_ => NonEmptyWrap(2))

NonEmptyWrap(2).map_2(_ * 2)
NonEmptyWrap(1).map_2(_ * 10)

NonEmptyWrap(2).map_2("=" + _.toString )
EmptyWrap.map_2(_ => "efeffr44f4f44bf")

NonEmptyWrap(30).withFilter(_ >= 0)
NonEmptyWrap(-30).withFilter(_ >= 0)
EmptyWrap.withFilter( _ => 0 == 0 )