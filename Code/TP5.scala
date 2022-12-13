
//EX1
enum Tree[+A]:
    case Leaf(label: A)
    case Branch(left: Tree[A], right: Tree[A])

object Tree:

    def size[A](t: Tree[A]): Int = t match
        case Leaf(_) => 1
        case Branch(l, r) => size(l) + size(r) + 1


import Tree._
val ti = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Leaf(4)))
val ts = Branch(Branch(Leaf("01"),Leaf("02")),Branch(Leaf("03"),Leaf("04")))


object Tree:
    def biggestInt(t: Tree[Int]): Int = t match
        case Leaf(i) => i
        case Branch(l, r) => biggestInt(l).max(biggestInt(r))

/*
scala> biggestInt(ti)
val res0: Int = 4
*/

//EX2

    def firstPositive(t: Tree[Int]): Option[Int] = t match
    case Leaf(i) => if i > 0 then Some(i) else None
    //case Branch(l, r) => firstPositive(l).orElse(firstPositive(r))
    case Branch(l, r) => firstPositive(l) orElse firstPositive(r)

/*
scala> firstPositive(ti)
val res0: Option[Int] = Some(1)
scala> firstPositive(Leaf(-5))
val res1: Option[Int] = None
*/


    extension (t: Tree[Int])
// We extend objects of type Tree[Int] with a method biggestInt.
// By definition, an object of type Tree[Int] has at least one Int.

        def biggestInt: Int = t match
            case Leaf(i) => i
            case Branch(l, r) => l.biggestInt.max(r.biggestInt)
            // We extend objects of type Tree[Int] with a method firstPositive.
            // Trees of type Tree[Int] could contain no positive integers.
            // We therefore need to manage those exceptions!
        def firstPositive: Option[Int] = t match
            case Leaf(i) => if i > 0 then Some(i) else None
            case Branch(l, r) => l.firstPositive.orElse(r.firstPositive)

    extension (t: Tree[String])
            // We extend objects of type Tree[String] with a method esrever.
            // By definition, an object of type Tree[String] has at least one String.

        def esrever: Tree[String] = t match
            case Leaf(i) => Leaf(i.reverse)
            case Branch(l, r) => Branch(l.esrever, r.esrever)

//EX4

enum Either[+E, +A]:
    case Left(get: E)
    case Right(get: A)

    def toList: List[A] = this match
        case Right(a) => List(a)
        case Left(_) => List()

    def map[B](f: A => B): Either[E, B] = this match
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
        case Left(e) => Left(e)
        case Right(a) => f(a)

    def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match
        case Left(_) => alt
        case Right(a) => Right(a)

    def getOrElse[B >: A](default: => B): B = this match
        case Left(_) => default
        case Right(a) => a

    def toOption: Option[A] = this match
        case Left(_) => None
        case Right(a) => Some(a)

//EX5
import Either._
def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch case e: Exception => Left(e)

/*
scala> safeDiv(10,2)
val res0: Either[Exception, Int] = Right(5)

scala> safeDiv(10,2).map(x => x + 2)
val res1: Either[Exception, Int] = Right(7)

scala> safeDiv(10,2).map(x => x + 2).getOrElse("Oops")
val res2: Matchable = 7

scala> safeDiv(10,2).map(x => x + 2).orElse(Left("Oops"))
val res3: Either[Object, Int] = Right(7)

scala> safeDiv(10,2).flatMap(safeDiv(_, 2))
val res4: Either[Exception, Int] = Right(2)

scala> safeDiv(10,0)
val res5: Either[Exception, Int] = Left(java.lang.ArithmeticException: / by zero)

scala> safeDiv(10,0).map(x => x + 2)
val res6: Either[Exception, Int] = Left(java.lang.ArithmeticException: / by zero)

scala> safeDiv(10,0).map(x => x + 2).getOrElse("Oops")
val res7: Matchable = Oops

scala> safeDiv(10,0).map(x => x + 2).orElse(Left("Oops"))
val res8: Either[Object, Int] = Left(Oops)

scala> safeDiv(10,0).flatMap(safeDiv(_, 2))
val res9: Either[Exception, Int] = Left(java.lang.ArithmeticException: / by zero)
*/

def safeFib(n: Int): Either[String, Int] =
    if n < 0 then Left("Argument is negative.")
    else if n < 2 then Right(n)
    else safeFib(n - 1).flatMap(a => safeFib(n - 2).map(b => a + b))


def fib(n: Int): Int =
    if n < 2 then n
    else fib(n - 1) + fib(n - 2)

def fib0 = lift(fib)

/*
scala> val x = List((2,3),(2,2),(2,1),(2,0)).map((x,y) => safeDiv(x, y)).map(fib0)
val x: List[Either[?, Int]] = List(
Right(0),
Right(1),
Right(1),
Left(java.lang.ArithmeticException: / by zero))
*/

def safeFib2(n: Int): Either[String, Int] =
    if n < 0 then Left("Argument is negative.")
    else if n < 2 then Right(n)
    //else safeFib(n - 1).flatMap(a => safeFib(n - 2).map(b => a + b))

    for
        a <- safeFib(n - 1) // represents the outer flatMap
        b <- safeFib(n - 2) // represents the inner map

    yield a + b // the expression to be computed

def safeFib3(n: Int): Either[String, Int] =
    @annotation.tailrec
    def helper(n: Int, a: Int, b: Int): Either[String, Int] =
        if n < 0 then Left("Argument is negative.")
        if n == 0 then Right(a)
        else helper(n - 1, b, a + b)
        
    helper(n, 0, 1)