package module1

import scala.annotation.tailrec
import scala.language.postfixOps


/**
 * referential transparency
 */


object referential_transparency {

  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification {
    case class Email(email: String, text: Html) extends Notification

    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService {
    def sendNotification(notification: Notification): Unit

    def createNotification(abiturient: Abiturient): Notification
  }


  trait AbiturientService {

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

}


// recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int = if (n == 0) 1 else n * factRec(n - 1)

  def tailRec(n: Int): Int = {
    @tailrec
    def loop(i: Int, accum: Int): Int = if (i == 0) accum else loop(i - 1, n * accum)

    loop(n, 1)
  }


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */

  //реализация
  def fibonachi(n: Int): Seq[Int] = {
    @tailrec
    def inner(cur: Int, prev: Int, sumFib: (Int, Seq[Int])): (Int, Seq[Int]) = {
      cur match {
        case 0 => (prev, sumFib._2)
        case _ => inner(cur - 1, sumFib._1, (prev + sumFib._1, prev +: sumFib._2))
      }
    }

    val fibC = inner(n, 0, (1, Seq()))
    (fibC._1 +: fibC._2).reverse
  }


}

object hof {


  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    result
  }

  def doomy(string: String) = {
    Thread.sleep(1000)
    println(string)
  }



  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  val isEven: Int => Boolean = not(isOdd)

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)


  // изменение самой функции

  def sum(x: Int, y: Int): Int = x + y

  val s1: Int => Int = partial(5, sum)

  s1(3) // 8

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = f.curried(a)


  trait Consumer {
    def subscribe(topic: String): LazyList[Record]
  }

  case class Record(value: String)

  case class Request()

  object Request {
    def parse(str: String): Request = ???
  }

  /**
   *
   * (Опционально) Реализовать ф-цию, которая будет читать записи Request из топика,
   * и сохранять их в базу
   */
  def createRequestSubscription() = ???

}


/**
 * Реализуем тип Option
 */


object opt {

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // + covariant Option[Animal] родитель для Option[Dog]
  // invariant Option[Animal] нет связи Option[Dog]
  // - contravariant связь наоборот между Option[Animal] и Option[Dog]

  class Animal

  class Dog extends Animal

  def findAnimal: Option[Animal] = ???

  def findDog: Option[Dog] = ???

  def treat(animal: Animal): Unit = ???

  def treat(animal: Option[Animal]): Unit = ???

  val animal: Animal = ???
  val dog: Dog = ???
  treat(animal)
  treat(dog)

  def divide(x: Int, y: Int): Option[Int] = {
    if (y == 0) None
    else Some(x / y)
  }


  val opt: Option[Int] = ???

  val opt2: Option[Int] = opt.flatMap(i => Option(i + 1))
  val opt3 = opt.map(i => i + 1)


  object Option {
    def apply[T](v: T): Option[T] = Some(v)
  }


  //////////////// Домашнее задание  //////////////////


  sealed trait Option[+T] {

    def isEmpty: Boolean = this match {
      case None => true
      case Some(v) => false
    }


    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("get on empty Option")
    }

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    // методы реализованные в домашнеем задании

    def printIfAny: Unit = this match {
      case None => ()
      case Some(v) => print(v)
    }

    def zip[T1 >: T, B](that: Option[B]): Option[(T1, B)] = (this, that) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _ => None
    }

    def filter(p: T => Boolean): Option[T] = this match {
      case None => None
      case Some(v) => if (p(v)) Some(v) else None
    }

    // другие реализации

    def filter_2(p: T => Boolean): Option[T] = this match {
      case Some(v) if p(v) => this
      case _ => None
    }

    def filter_3(p: T => Boolean): Option[T] = flatMap(v => if (p(v)) Some(v) else None)

    //опционно
    def isDefined: Boolean = !isEmpty


  }

  case class Some[T](v: T) extends Option[T]

  case object None extends Option[Nothing]


  ////////////// Задание ////////////////////////
  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */

  //реализовано в sealed trait Option выше


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */

  //реализовано в sealed trait Option выше

  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

  //реализация filter в sealed trait Option выше
  //альтернативная реализация filter_2
  //альтернативная реализация filter_3


  // Тест реализаций в методе main
  ///////////////////////////////////////////////////

}

object list {

  Cons(1, Nil) // List(1)
  Cons(1, Cons(2, Nil)) // List(1, 2, 3)
  Cons(1, Cons(2, Cons(3, Nil))) // List()


  List(1, 2)


  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
   */

  /////////////// Домашнее задание //////////////

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

      recursion(v.reverse, Nil)

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
      def rec(l: List[TT], acc: List[B]): List[B] = l match {
        case Nil => acc
        case Cons(head, tail) => rec(tail, f(head) :: acc)
      }

      rec(this, Nil).reverse
    }

    //// альтернативная реализация map через foldRight ///////////////////
    // реализуем стекобезопвсный foldLeft
    @tailrec
    final def foldLeft[TT >: T, B](l: List[TT], b: B)(f: (B, TT) => B): B = l match {
      case Nil => b
      case Cons(h, t) => foldLeft(t, f(b, h))(f)
    }

    //реализуем foldRight на foldLeft
    def foldRight[TT >: T, B](l: List[TT], z: B)(f: (TT, B) => B): B =
      foldLeft(l.reverse, z)((b, a) => f(a, b))

    //реализуем map на foldRight
    def map_2[TT >: T, B](f: TT => B): List[B] =
      foldRight(this, Nil: List[B])((h, t) => Cons(f(h), t))
    ///////////////////////////////////////////////////////////////////



    def filter[TT >: T](p: TT => Boolean): List[TT] = {
      def rec(l: List[TT], acc: List[TT]): List[TT] = l match {
        case Nil => acc
        case Cons(h, t) => rec(t, if (p(h)) h :: acc else acc)
      }

      rec(this, Nil).reverse
    }

    //альтернативная реализация на foldRight
    def filter_2[TT >: T](p: TT => Boolean): List[TT] =
      foldRight(this, Nil: List[TT])((h, t) => if (p(h)) Cons(h, t) else t)

  }

  case class Cons[A](head: A, tail: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  object List {
    def apply[A](v: A*): List[A] =
      if (v.isEmpty) Nil
      else Cons(v.head, apply(v.tail: _*))
  }


  /**
   * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
   *
   */

  //реализовано в sealed trait List выше

  /**
   * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
   *
   */
  //реализовано в sealed trait List выше

  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   * Для этого можно воспользоваться *
   *
   * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
   * def printArgs(args: Int*) = args.foreach(println(_))
   */

  //метод init реализован в sealed trait List выше

  /**
   *
   * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
   */

  //реализовано в sealed trait List выше

  /**
   *
   * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
   */

  //реализация map через рекурсию и паттерн-матчинг  в sealed trait List выше
  //созданы  реализации foldLeft и foldRight
  //реализация map_2  на foldRight


  /**
   *
   * Реализовать метод filter для списка который будет фильтровать список по некому условию
   */
  //реализация filter через рекурсию и паттерн-матчинг  в sealed trait List выше
  //альтернативная реализация filter_2 на foldRight


  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */
 // реализация
 def incList(l: List[Int]): List[Int] = l map[Int,Int] (_ + 1)

  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */
  //реализация
  def shoutString(l: List[String]): List[String] = l map[String,String] ("!" + _)

  // Тест реализаций в методе runTest объекта CustomListTest. Вызывается из метода main.
  ////////////////////////////////////////////////////////////////////////////////////////

}