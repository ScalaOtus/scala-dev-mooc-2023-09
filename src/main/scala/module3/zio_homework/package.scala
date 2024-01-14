package module3

import zio.{Has, IO, Task, ULayer, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */
  lazy val guessProgram = {
    def readNumberFromConsole(console: Console.Service): Task[Int] = {1
      lazy val readLine: IO[IOException, String] = console.getStrLn
      lazy val readInt: Task[Int] = readLine.flatMap(str => ZIO.effect(str.toInt))
      lazy val readIntOrRetry: Task[Int] = readInt.orElse {
        ZIO.effect(println("Некорректный ввод, попробуйте снова")).zipRight(readIntOrRetry)
      }

      console.putStrLn("Введите число от 1 до 3 включительно: ") *> readIntOrRetry
    }

    def checkUsersNumber(usersNumber: Int, console: Console.Service): ZIO[Any, Throwable, Int] = {
      lazy val checkUsersNumber = if (usersNumber > 3 || usersNumber < 1) {
        console.putStrLn("Число должно быть от 1 до 3 включительно") *>
          ZIO.fail(new Throwable("Error"))
      } else ZIO.succeed(usersNumber)

      checkUsersNumber
    }

    for {
      console: Console.Service <- ZIO.environment[Console].map(_.get)
      random <- ZIO.environment[Random].map(_.get)
      numberToGuess <- random.nextIntBetween(1, 4)
      _ <- console.putStrLn("Задуманное число: " + numberToGuess.toString)
      usersNumber <- readNumberFromConsole(console)
      usersNumberProcessed <- checkUsersNumber(usersNumber, console)
      _ <- console.putStr(if (usersNumberProcessed == numberToGuess) "Верно! Вы угадали загаданное число" else s"Неверно! Загаданное число: ${numberToGuess}\n")
    } yield ()


  }

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile = ???

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: ZIO[Any, Throwable, config.AppConfig] = for {
    c <- config.load.foldM(
      _ => ZIO.succeed(config.AppConfig("127.0.0.1", "8080")),
      success => ZIO.succeed(success)
    )
    _ <- ZIO.effect(println(c.host, c.port))
  } yield c


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = ???

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = ???


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = ???


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = ???


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */


  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg = ???

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp = ???

}
