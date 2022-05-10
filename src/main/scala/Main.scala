import PrintableInstances.given
import Tree.{*, given}
import cats.syntax.*
import cats.syntax.eq.*
import cats.syntax.functor.*

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.*
import scala.concurrent.duration.*
import scala.util.Try

@main def hello: Unit =
  println("Ch1")
  println(Printable.show("s1"))
  println(Printable.show(11))
  Printable.print("s2")
  Printable.print(22)
  val cat = Cat("Alex", 2, "black")
  println(cat.show)
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  println(cat1 === cat1)
  println(cat1 =!= cat2)
  println(optionCat1 === optionCat1)
  println(optionCat1 =!= optionCat2)
  println("Ch2")
  println(add(List(1, 2, 3)))
  println("Ch3")
  println(branch(leaf(10), leaf(20)).map(_ * 2))
  println(Printable[String].contramap[Int](_.toString + "!").show(23))
  println(Box("hello world").show)
  println(Box(true).show)
  println(summon[Codec[Double]].encode(124.3))
  println(summon[Codec[Double]].decode("342.1"))
  println("Ch4")
  println(validateAdult[Try](18))
  println(validateAdult[Try](8))
  type ExceptionOr[A] = Either[Throwable, A]
  println(validateAdult[ExceptionOr](-1))
  println(foldRight((1 to 100000).toList, 0L)(_ + _))
  //  factorial(5)
//  Await.result(Future.sequence(Vector(
//    Future(factorial(5)),
//    Future(factorial(5))
//  )), 5.seconds)
//  val fr = Await.result(Future.sequence(Vector(
//    Future(factorialW(5)),
//    Future(factorialW(5))
//  )).map(_.map(_.written)), 5.seconds)
//  println(fr)
  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))
  println(evalOne("42").runA(Nil).value)
  val program = for
    _<- evalOne("1")
    _<- evalOne("2")
    ans <- evalOne("+")
  yield ans
  println(program.runA(Nil).value)
  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
  println(multistageProgram.runA(Nil).value)
