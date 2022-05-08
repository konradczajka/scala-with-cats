import cats.Functor
import cats.syntax.functor.*

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree:
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  given Functor[Tree] with
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
      case Branch(left, right) => Branch(left.map(f), right.map(f))
      case Leaf(value) => Leaf(f(value))

final case class Box[A](value: A)

given [A: Printable]: Printable[Box[A]] = Printable[A].contramap(_.value)

trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] =
    new Codec[B]:
      def encode(value: B): String =
        self.encode(enc(value))
      def decode(value: String): B =
        dec(self.decode(value))
}

given Codec[String] with
  def encode(value: String): String = value
  def decode(value: String): String = value

given Codec[Double] = summon[Codec[String]].imap(_.toDouble, _.toString)
