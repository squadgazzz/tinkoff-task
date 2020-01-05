/**
 * На вход List[IO[String]]
 * Получить IO[(List[String], List[Throwable]) - результат агрегации выполненых IO и исключений
 */

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration._

object SuccessAndFalse extends IOApp {

  val talk = List(
    IO.sleep(1.second).as("red"),
    IO.raiseError(new RuntimeException("exception1")),
    IO.pure("blue"),
    IO.raiseError(new RuntimeException("exception2")),
    IO.pure("green"),
    IO.raiseError(new RuntimeException("exception3"))
  )

  // получить IO[(List[Throwable], IO[String])]
  def collectErrors[A](in: List[IO[A]]): IO[(List[Throwable], List[A])] =
    in.map(_.attempt).sequence.map(e =>
      (
        e.collect {
          case Left(t) => t
        },
        e.collect {
          case Right(v) => v
        }
      )
    )

  // распечатать все сообщения об ошибках
  def run(args: scala.List[String]): IO[ExitCode] = {
    collectErrors(talk)
      .map(p => p._1.map(_.getMessage))
      .flatMap(s => IO(s.foreach(println)))
      .as(ExitCode.Success)
  }
}

/** ComplexNumber
 * Написать реализацию комплексного числа,
 * такую, чтобы можно было его создавать следующим образом:
 * val cmp = 1 + 3 * I
 * И были возможны операции
 * cmp + N  - увеличение реальной части на N, где n любое рациональное число
 * cmp + N*I - увеличение мнимой части
 * сmp + (1 + 4*I) - сложение 2-х комплексых чисел
 * */
object ComplexNumber extends App {

  val I = Complex(0, 1)

  // solution
  case class Complex(re: Double, im: Double) {
    def this(re: Double) = this(re, 0)

    def +(c: Complex): Complex = Complex(re + c.re, im + c.im)

    def +(d: Double): Complex = Complex(re + d, im)

    def *(c: Complex): Complex = Complex(re * c.re - im * c.im, im * c.re + re * c.im)

    override def toString: String = s"$re + $im * i"
  }

  object Complex {
    def apply(re: Double) = new Complex(re)

    implicit def fromDouble(d: Double): Complex = Complex(d)
  }

  println(1 + 3 * I) // should print  "1 + 3 * i"
}

