package module1.utils
import module1.list._
object CustomListTest {

  def runTest(): Unit = {

    println("\n***************************  List  *****************************")
    println("метод ::")
    println(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)
    println("один" :: "два" :: "три"  :: Nil)
    println("один" :: 2 :: "три"  :: Nil)

    println("\nmkString")
    println(List(1, 2, 3, 4, 5, 6).mkString(","))
    println(List(1, "два", 3, "четыре").mkString("#$"))

    println("\ninit")
    println(List().init(1, 2, 3, 4))
    println(List().init(1, "rfr", 3, 4))
    println(List().init())

    println("\nreverse")
    println(List(1, 2, 3, 4, 5).reverse)
    println(List(1, 2, "три", 4, 5).reverse)

    println("\nmap")
    println("реализация map")
    println(List(1,2,3,4,5).map[Int,Int](_ * 2))
    println("реализация map_2 на foldRight")
    println(List(1, 2, 3, 4, 5).map_2[Int, Int](_ * 2))

    println("\nfilter")
    println("реализация filter")
    println(List(1, 2, 3, 4, 5).filter(_ < 3))
    println(List(1, 2, 3, 4, 5).filter(_ >= 3))

    println("реализация filter_2 на foldRight")
    println(List(1, 2, 3, 4, 5).filter_2(_ < 3))
    println(List(1, 2, 3, 4, 5).filter_2(_ >= 3))

    println("\nincList")
    println(incList(List(1,2,3,4,5)))

    println("\nshoutString")
    println(shoutString( List("one", "two", "three")))









  }

}
