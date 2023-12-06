//EX 1 
/*
scala> { println("1") ; true } && { println("2") ; true } && { println("3") ; true }
1
2
3
val res0: Boolean = true
scala> { println("1") ; true } && { println("2") ; false } && { println("3") ; true }
1
2
val res1: Boolean = false
scala> { println("1") ; true } || { println("2") ; true } || { println("3") ; true }
1
val res2: Boolean = true
scala> { println("1") ; false } || { println("2") ; true } || { println("3") ; true }
1
2
val res3: Boolean = true
*/

//EX2 
def myif[A](test: Boolean, onIf: => A, onElse: => A): A =
    if test then onIf else onElse

def myif2[A](test: Boolean)(onIf: => A)(onElse: => A): A =
    if test then onIf else onElse
    

/*
scala> myif(3 > 5, 2 / 0, 5)
val res0: Int = 5
scala> myif2(3 > 5)(2 / 0)(5)
val res1: Int = 5
*/

//EX3
def myifelseifelse[A](t1: Boolean, i: => A, t2: => Boolean, ei: => A, e: => A): A =
    myif(t1, i, myif(t2, ei, e))

//EX4
def foo(x: => Int): Int =
    x + x

/*
scala> foo({ println("bar") ; 2 })
bar
bar
val res3: Int = 4
*/


//EX5
import Flux.*

enum Flux[+A]:
    case Empty
    case Cons(h: () => A, t: () => Flux[A])

    def headOption: Option[A] = this match
        case Empty => None
        case Cons(h, t) => Some(h())

    def toList: List[A] = this match
        case Cons(h,t) => h() :: t().toList
        case Empty => Nil
        
    def take(n: Int): Flux[A] = this match
        case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
        case Cons(h, _) if n == 1 => cons(h(), empty)
        case _ => empty
        
    def filter(f: A => Boolean): Flux[A] = this match
        case Cons(h, t) if f(h()) => cons(h(), t().filter(f))
        case Cons(_, t) => t().filter(f)
        case _ => empty
        
    def map[B](f: A => B): Flux[B] = this match
        case Cons(h, t) => cons(f(h()), t().map(f))
        case _ => empty
        
    //Solution 
    def takeWhile(p: A => Boolean): Flux[A] = this match
        case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
        case _ => empty
        
    def exists(p: A => Boolean): Boolean = this match
        case Cons(h, t) => p(h()) || t().exists(p)
        case _ => false
        
    def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
        case Cons(h,t) => f(h(), t().foldRight(acc)(f))
        case _ => acc

    //End of solution 
        
object Flux:
    def cons[A](hd: => A, tl: => Flux[A]): Flux[A] =
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)

    def empty[A]: Flux[A] = Empty

    def apply[A](as: A*): Flux[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail*))

        