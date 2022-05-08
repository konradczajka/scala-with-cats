import PrintableInstances.given
import Tree.{given, *}
import cats.syntax.*
import cats.syntax.eq.*
import cats.syntax.functor.*

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
  println(cat1 ===  cat1)
  println(cat1 =!=  cat2)
  println(optionCat1 ===  optionCat1)
  println(optionCat1 =!=  optionCat2)
  println("Ch2")
  println(add(List(1, 2, 3)))
  println("Ch3")
  println(branch(leaf(10), leaf(20)).map(_ * 2))
  println(Printable[String].contramap[Int](_.toString + "!").show(23))
  println(Box("hello world").show)
  println(Box(true).show)
  println(summon[Codec[Double]].encode(124.3))
  println(summon[Codec[Double]].decode("342.1"))

