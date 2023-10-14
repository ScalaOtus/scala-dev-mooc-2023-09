import scala.annotation.tailrec


def fibonachi(n: Long): Seq[Long] = {
  @tailrec
  def inner(cur: Long, prev: Long, sumFib: (Long, Seq[Long])): (Long, Seq[Long]) = {
    cur match {
      case 0 => (prev, sumFib._2)
      case _ => inner(cur - 1, sumFib._1, (prev + sumFib._1, prev +: sumFib._2))
    }

  }

  val fibC = inner(n, 0, (1, Seq()))
  (fibC._1 +: fibC._2).reverse
}

fibonachi(0)
fibonachi(1)
fibonachi(2)
fibonachi(3)


sealed trait List[+T] {
  def ::[TT >: T](elem: TT): List[TT] = Cons(elem, this)

  def mkString(delimiter: String): String = {
    @tailrec
    def concat[TT >: T](l: List[TT], acc: String): String = l match {
      case Nil => acc
      case Cons(head, tail) =>
        concat(tail, head.toString.reverse + delimiter.reverse + acc)
    }
    concat(this, "").dropRight(delimiter.length).reverse
  }

  def init[TT >: T](v: TT*): List[TT] = {
    @tailrec
    def recursion(seq: Seq[TT], acc: List[TT]): List[TT] = seq match {
      case Seq() => acc
      case head +: tail => recursion(tail, Cons(head, acc))
    }

    recursion(v.reverse,Nil)

  }

  def reverse[TT >: T]: List[TT] = {
    @tailrec
    def rev(l: List[TT], acc: List[TT]): List[TT] = l match {
      case Nil => acc
      case Cons(head, tail) => rev(tail, head :: acc)
    }

    rev(this, Nil)
  }

  def map[TT >: T, B](f: TT => B): List[B] = {
    @tailrec
    def recursion(l: List[TT], acc: List[B]): List[B] = l match {
      case Nil => acc
      case Cons(head, tail) => recursion(tail, f(head) :: acc)
    }

    recursion(this, Nil).reverse
  }

  @tailrec
  final def foldLeft[TT >: T, B](l: List[TT], z: B)(f: (B, TT) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldRight[TT >: T, B](l: List[TT], z: B)(f: (TT, B) => B): B =
    foldLeft(l.reverse, z)((b, a) => f(a, b))


  def map_2[TT >: T, B](f: TT => B): List[B] =
    foldRight(this, Nil: List[B])((h, t) => Cons(f(h), t))


  def filter[TT >: T](p:TT => Boolean): List[TT] = {
    def rec(l: List[TT], acc: List[TT]): List[TT] = l match {
      case Nil => acc
      case Cons(h, t) => rec(t, if (p(h)) h :: acc else acc)
    }
    rec(this, Nil).reverse
  }

  def filter_2[TT >: T](p: TT => Boolean): List[TT] =
    foldRight (this, Nil: List[TT])((h, t) => if (p(h)) Cons(h, t) else t)



}

case class Cons[A](head: A, tail: List[A]) extends List[A]

case object Nil extends List[Nothing]

object List {
  def apply[A](v: A*): List[A] =
    if (v.isEmpty) Nil
    else Cons(v.head, apply(v.tail: _*))

}

def incList(l: List[Int]): List[Int] = l map[Int,Int] (_ + 1)

def shoutString(l: List[String]): List[String] = l map[String,String] ("!" + _)


5 :: List(1, 2, 3)

List(1, 2, 3).mkString(",")
List(1, "два", 3, "четыре").mkString("#$")
List().init(1,2,3,4)
List().init(1,"rfr",3,4)
List().init()
List(1,2,3,4,5).reverse
List(1,2,"три",4,5).reverse
List(1,2,3,4,5).map[Int,Int](_ * 2)
List(1,2,3,4,5).map_2[Int,Int](_ * 2)
List(1,2,3,4,5).filter(_ < 3)
List(1,2,3,4,5).filter(_ >= 3)
List(1,2,3,4,5).filter_2(_ < 3)
List(1,2,3,4,5).filter_2(_ >= 3)

incList( List (1,2,3,4,5))
shoutString( List("one", "two", "three"))





