package module1.datacollections.homework


import scala.annotation.unused
import scala.collection.immutable.Queue
import scala.language.postfixOps

/*
class BallsExperiment {

  def isFirstBlackSecondWhite(): Boolean = {
    ???
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = ???
    val countOfExperiments = ???
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}*/

object BallsTest {

  trait RNG {
    def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  }

  private type Rand[+A] = RNG => (A, RNG)

  private case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  private object RngFunctions {

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, r1) = f(rng)
        g(a)(r1)
      }

    private def unit[A](a: A): Rand[A] = rng => (a, rng)

    private def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }
  }


  trait Ball

  private case object Black extends Ball

  private case object White extends Ball

  case class ExperimentResult(ball1: Option[Ball] = None, ball2: Option[Ball] = None,
                              firstWhite: Option[Boolean] = None, firstBlack: Option[Boolean] = None,
                              secondWhiteWhite: Option[Boolean] = None, secondBlackWhite: Option[Boolean] = None,
                              secondWhiteBlack: Option[Boolean] = None, secondBlackBlack: Option[Boolean] = None,
                              existWhite: Option[Boolean] = None, existBlack: Option[Boolean] = None)


  @unused
  case class BallsExperiment(balls: Vector[Ball], result: ExperimentResult) {
    override def toString: String = {

      val resultStr: String = result match {
        case ExperimentResult(Some(b1), None, Some(f_w), Some(f_b), None, None, None, None, None, None) =>
          b1.toString + " | Первый " + (if (f_w) "белый" else "") + (if (f_b) "черный" else "")
        case ExperimentResult(Some(b1), Some(b2), Some(f_w), Some(f_b), Some(s_w_a_w), Some(s_w_a_b), Some(s_b_a_w), Some(s_b_a_b), Some(e_w), Some(e_b)) =>
          b1.toString + " " + b2.toString + " | Первый " + (if (f_w) "белый" else "") + (if (f_b) "черный" else "") + " | " +
            "Второй " + (if (s_w_a_w) "белый после белого" else "") + (if (s_w_a_b) "белый после черного" else "") +
            (if (s_b_a_w) "черный после белого" else "") + (if (s_b_a_b) "черный после черного" else "") + " | " +
            (if (e_w) "белый выбран " else "") + (if (e_b) "черный выбран" else "")

      }

      "<" + balls.mkString(",") + ">" + " - " + resultStr
    }

    def experiment1: (Ball, BallsExperiment) = {
      val indexBall: Int = random(6)
      val ball: Ball = this.balls(indexBall)
      val (a, b) = this.balls splitAt indexBall
      val vect: Vector[Ball] = if (b != Nil) a ++ b.tail else a

      val result = this.result.copy(
        ball1 = Some(ball),
        firstWhite = Some(ball == White),
        firstBlack = Some(ball == Black)
      )

      (ball, BallsExperiment(vect, result))
    }

    def experiment2: (Ball, BallsExperiment) = {
      val indexBall: Int = random(5)
      val ball = this.balls(indexBall)
      val (a, b) = this.balls splitAt indexBall
      val vect: Vector[Ball] = if (b != Nil) a ++ b.tail else a

      val result = this.result.copy(
        ball2 = Some(ball),
        secondWhiteWhite = (this.result.ball1, ball) match {
          case (None, _) => None
          case (Some(White), White) => Some(true)
          case _ => Some(false)
        },
        secondBlackWhite = (this.result.ball1, ball) match {
          case (None, _) => None
          case (Some(Black), White) => Some(true)
          case _ => Some(false)
        },
        secondBlackBlack = (this.result.ball1, ball) match {
          case (None, _) => None
          case (Some(Black), Black) => Some(true)
          case _ => Some(false)
        },
        secondWhiteBlack = (this.result.ball1, ball) match {
          case (None, _) => None
          case (Some(White), Black) => Some(true)
          case _ => Some(false)
        },
        existWhite = (this.result.ball1, ball) match {
          case (None, _) => None
          case (Some(White), _) => Some(true)
          case (Some(_), White) => Some(true)
          case _ => Some(false)
        },
        existBlack = (this.result.ball1, ball) match {
          case (None, _) => None
          case (Some(Black), _) => Some(true)
          case (Some(_), Black) => Some(true)
          case _ => Some(false)
        }
      )

      (ball, BallsExperiment(vect, result))
    }

  }

  val time: Long = java.time.Instant.now().getEpochSecond
  private var rng: RNG = Simple(time)

  private def random(end: Int): Int = {
    val rand01: Rand[Int] = RngFunctions.nonNegativeLessThan(end)
    val rPair: (Int, RNG) = rand01(rng)
    val value = rPair._1
    rng = rPair._2
    value
  }

  private def createExperiment(): BallsExperiment = {
    var qWhite: Queue[Ball] = Queue(White, White, White)
    var qBlack: Queue[Ball] = Queue(Black, Black, Black)
    var balls: Vector[Ball] = Vector()

    0 to 5 foreach { _ =>
      val rnd01 = random(2)

      val next: (Ball, Queue[Ball]) = rnd01 match {
        case 0 => if (qWhite.isEmpty) qBlack.dequeue else qWhite.dequeue
        case 1 => if (qBlack.isEmpty) qWhite.dequeue else qBlack.dequeue
      }

      next._1 match {
        case White => qWhite = next._2
        case Black => qBlack = next._2
      }
      balls = balls :+ next._1
    }

    BallsExperiment(balls, ExperimentResult())

  }


  def main(args: Array[String]): Unit = {
    println("Balls Experiment")

    val count = 1000000
    var listOfExperiments: List[BallsExperiment] = List()
    1 to count foreach { _ =>

      val newExperiment: BallsExperiment = createExperiment()
      listOfExperiments = newExperiment +: listOfExperiments
    }

    val listAfter1: List[BallsExperiment] = listOfExperiments map { x =>
      val res1: (Ball, BallsExperiment) = x.experiment1
      res1._2
    }

    val listAfter2: List[BallsExperiment] = listAfter1 map { x =>
      val res2: (Ball, BallsExperiment) = x.experiment2
      res2._2
    }

    val firstWhiteCount =  listAfter2 map (x => if (x.result.firstWhite.get) 1D else 0D) reduce((a, b) => a + b )
    val firstBlackCount =  listAfter2 map (x => if (x.result.firstBlack.get) 1D else 0D) reduce((a,b) => a + b )
    val secondWhiteWhiteCount = listAfter2 map (x => if (x.result.secondWhiteWhite.get) 1D else 0D) reduce((a, b) => a + b )
    val secondBlackWhiteCount = listAfter2 map (x => if (x.result.secondBlackWhite.get) 1D else 0D) reduce((a, b) => a + b )
    val secondWhiteBlackCount = listAfter2 map (x => if (x.result.secondWhiteBlack.get) 1D else 0D) reduce((a, b) => a + b )
    val secondBlackBlackCount = listAfter2 map (x => if (x.result.secondBlackBlack.get) 1D else 0D) reduce((a, b) => a + b )
    val existWhite = listAfter2 map (x => if (x.result.existWhite.get) 1D else 0D) reduce((a, b) => a + b )
    val existBlack = listAfter2 map (x => if (x.result.existBlack.get) 1D else 0D) reduce((a, b) => a + b )

    val listFirstWhite = listAfter2 filter(x => x.result.ball1.get == White)
    val listFirstBlack = listAfter2 filter (x => x.result.ball1.get == Black)

    val secondWhite_afterWhite_count = listFirstWhite map (x => if (x.result.secondWhiteWhite.get) 1D else 0D) reduce ((a, b) => a + b)
    val secondBlack_afterWhite_count = listFirstWhite map (x => if (x.result.secondWhiteBlack.get) 1D else 0D) reduce ((a, b) => a + b)
    val secondWhite_afterBlack_count = listFirstBlack map (x => if (x.result.secondBlackWhite.get) 1D else 0D) reduce ((a, b) => a + b)
    val secondBlack_afterBlack_count = listFirstBlack map (x => if (x.result.secondBlackBlack.get) 1D else 0D) reduce ((a, b) => a + b)




    println(s"Вероятность в ПЕРВОМ эксперименте выбран белый  = ${firstWhiteCount / count}")
    println(s"Вероятность в ПЕРВОМ эксперименте выбран черный = ${firstBlackCount / count}")

    println(s"Совместная вероятность Первый белый, Второй белый  = ${secondWhiteWhiteCount / count}")
    println(s"Совместная вероятность Первый черный, Второй белый = ${secondBlackWhiteCount / count}")
    println(s"Совместная вероятность Первый белый, Второй черный = ${secondWhiteBlackCount / count}")
    println(s"Совместная вероятность Первый черный, Второй черный = ${secondBlackBlackCount / count}")

    println(s"Условная вероятность во ВТОРОМ эсперимете выбран белый при условии что в ПЕРВОМ выбран белый = ${secondWhite_afterWhite_count / firstWhiteCount}")
    println(s"Условная вероятность во ВТОРОМ эсперимете выбран черный при условии что в ПЕРВОМ выбран белый = ${secondBlack_afterWhite_count / firstWhiteCount}")
    println(s"Условная вероятность во ВТОРОМ эсперимете выбран белый при условии что в ПЕРВОМ выбран черный = ${secondWhite_afterBlack_count / firstBlackCount}")
    println(s"Условная вероятность во ВТОРОМ эсперимете выбран черный при условии что в ПЕРВОМ выбран черный = ${secondBlack_afterBlack_count / firstBlackCount}")

    println(s"Вероятность белый выбран  = ${existWhite / count}")
    println(s"Вероятность черный выбран = ${existBlack / count}")

  }

}