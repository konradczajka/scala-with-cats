import cats.*
import cats.implicits.*
import cats.syntax.*

trait Printable[A]:
  def show(a: A): String
  def contramap[B](func: B => A): Printable[B] =
    (b: B) => show(func(b))

extension[A] (self: A)
  def show(using p: Printable[A]): String = p.show(self)

object PrintableInstances:

  given Printable[String] with
    def show(a: String): String = s"'$a'"

  given Printable[Int] with
    def show(a: Int): String = a.toString

  given Printable[Boolean] with
    def show(a: Boolean): String = if a then "yes" else "no"

object Printable:
  def apply[A](using p: Printable[A]): Printable[A] = p
  def show[A: Printable](a: A): String = a.show
  def print[A: Printable](a: A): Unit = println(show(a))

case class Cat(name: String, age: Int, color: String)

import PrintableInstances.{ given }
given Printable[Cat] with
  def show(a: Cat) =
    s"${a.name.show} is a ${a.age.show} year-old ${a.color.show} cat"
//
//given Eq[Cat] with
//  def eqv(x: Cat, y: Cat): Boolean =
//    x.name === y.name && x.age === y.age && x.color === y.color

given Eq[Cat] = Eq.instance[Cat] { (x, y) => x.name === y.name && x.age === y.age && x.color === y.color }