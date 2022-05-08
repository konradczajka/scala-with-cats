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
