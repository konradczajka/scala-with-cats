import cats.data.*
import cats.implicits.*
import cats.*

trait Monad[F[_]]:
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(func.andThen(pure))

type Id[A] = A

given Monad[Id] with
  def pure[A](a: A): Id[A] = a

  def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

def x = 2.asRight[Int].ensure(-1)(_ > 5)

def validateAdult[F[_]](age: Int)(using em: MonadError[F, Throwable]): F[Int] =
  em.ensure(age.pure[F])(new IllegalArgumentException("Age must be greater than or equalto 18"))(_ >= 18)


//def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
//  as match
//    case head :: tail =>
//      fn(head, foldRight(tail, acc)(fn))
//    case Nil =>
//      acc

def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
  as match
    case head :: tail =>
      Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
    case Nil =>
      acc

def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  foldRightEval(as, Eval.now(acc)) { (a, b) =>
    b.map(fn(a, _))
  }.value

def slowly[A](body: => A) =
  try body finally Thread.sleep(100)

type Logged[A] = Writer[Vector[String], A]

def factorialW(n: Int): Logged[Int] =
  for
    ans <- if n == 0 then 1.pure[Logged] else slowly(factorialW(n - 1).map(_ * n))
    _ <- Vector(s"fact $n $ans").tell
  yield ans

case class Db(
               usernames: Map[Int, String],
               passwords: Map[String, String]
             )

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] =
  Reader(_.usernames.get(userId))

def checkPassword(username: String,
                  password: String): DbReader[Boolean] =
  Reader(_.passwords.get(username).exists(_ == password))

def checkLogin(userId: Int,
               password: String): DbReader[Boolean] =
  for
    username <- findUsername(userId)
    passwordMatches <- username.map(checkPassword(_, password))
      .getOrElse {
        false.pure[DbReader]
      }
  yield passwordMatches

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)
val passwords = Map(
  "dade" -> "zerocool",
  "kate" -> "acidburn",
  "margo" -> "secret"
)
val db = Db(users, passwords)

type CalcState[A] = State[List[Int], A]

//def operator(f: (Int, Int) => Int): State[List[Int], Int] =
//  for
//    a <- inspect[List[Int], Int](_.head)
//    _ <- modify[List[Int]](_.tail)
//    b <- inspect[List[Int], Int](_.head)
//    _ <- modify[List[Int]](_.tail)
//    ans <- f(a, b).pure[CalcState]
//    _ <- modify[List[Int]](_ :+ ans)
//  yield ans

def operator(func: (Int, Int) => Int): CalcState[Int] =
  State[List[Int], Int] {
    case b :: a :: tail =>
      val ans = func(a, b)
      (ans :: tail, ans)
    case _ =>
      sys.error("Fail!")
  }

def operand(x: Int): CalcState[Int] =
  State[List[Int], Int] {
    stack => (stack :+ x, x)
  }

def evalOne(sym: String): CalcState[Int] = sym match
  case "+" => operator(_ + _)
  case "-" => operator(_ - _)
  case "*" => operator(_ * _)
  case "/" => operator(_ / _)
  case x => operand(x.toInt)

def evalAll(input: List[String]): CalcState[Int] =
  input.foldLeft(0.pure[CalcState]) {
    (p, sym) => p.flatMap(_ => evalOne(sym))
  }
given Monad[Tree] with
  def pure[A](value: A): Tree[A] = Leaf(value)

  def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
    tree match {
      case Branch(l, r) =>
        Branch(flatMap(l)(func), flatMap(r)(func))
      case Leaf(value) =>
        func(value)
    }

  def tailRecM[A, B](a: A)
                    (func: A => Tree[Either[A, B]]): Tree[B] =
    flatMap(func(a)) {
      case Left(value) =>
        tailRecM(value)(func)
      case Right(value) =>
        Leaf(value)
    }
