import cats.Monoid
import cats.instances.int.*
import cats.syntax.semigroup.*

def add(items: List[Int]): Int =
//  items.foldLeft(Monoid[Int].empty)(_ |+| _)
  Monoid[Int].combineAll(items)