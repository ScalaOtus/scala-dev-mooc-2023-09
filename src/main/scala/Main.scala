
import module1.opt._
import module1.recursion.fibonachi

object Main {

  def main(args: Array[String]): Unit = {
    println("Hello world ")

    val t: Unit = true
    print(t)


    ////////////////// Домашнее задание //////////////
    println
    // реализация вычисления чисел фиббоначи
    println("\nFibonachi")
    (1 to 10) foreach (x => println(fibonachi(x)))

    println("\nOption")
    println(" printIfAny")
    val lAnyOPtion = List(Some("один"), None, Some(1), None, Some("два"), None, Some(2), None, Some("три"), None, Some(3))
    println(s" Test Option values = $lAnyOPtion")


    lAnyOPtion foreach { x =>
      x.printIfAny
      if (x.isDefined) println
    }


    println("\nzip")
    val zip1 = Some(1) zip Some("one")
    println(s" Some(1) zip Some(\"one\")  =  $zip1")
    val zip2 = None zip Some("one")
    println(s" None zip Some(\"one\") =  $zip2")
    val zip3 = Some(1) zip None
    println(s" Some(1) zip None  =  $zip3")
    val zip4 = Some(1) zip None
    println(s" None zip None  =  $zip4")


    println("\nfilter")
    val intListOt: List[Option[Int]] = List(None, Some(1), Some(-1), Some(2), Some(3), Some(-5), None)
    println(" value >= 0")
    intListOt.map(x => x.filter(_ >= 0)).collect { case Some(x) => x }.foreach(println)
    println(" value < 0")
    intListOt.map(x => x.filter(_ < 0)).collect { case Some(x) => x }.foreach(println)

    println("\nfilter_2")
    println(" value >= 0")
    intListOt.map(x => x.filter_2(_ >= 0)).collect { case Some(x) => x }.foreach(println)
    println(" value < 0")
    intListOt.map(x => x.filter_2(_ < 0)).collect { case Some(x) => x }.foreach(println)

    println("\nfilter_3")
    println(" value >= 0")
    intListOt.map(x => x.filter_3(_ >= 0)).collect { case Some(x) => x }.foreach(println)
    println(" value < 0")
    intListOt.map(x => x.filter_3(_ < 0)).collect { case Some(x) => x }.foreach(println)

  }


}