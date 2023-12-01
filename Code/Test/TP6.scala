/* Ex1: Explain call by name. Dans le cas du call by name, l'expression est évaluée uniquement 
lorsque cell-ci est appelée dans une fonction, et non lors de l'appel de la fonction. L'expression et 
de nouveau évaluée à chaque appel


Call by need: la variable est évaluée lorsqu'elle est invoquée. Par contre, sa valeur est stockée en cache
et elle ne sera pas re-évaluée lors de son prochain appel.

*/

// $ Call by name :
def callByName(x: => Int): Int = {
  println("Inside callByName function")
  x + x
}

val result = callByName {
  println("Evaluating x")
  42
}

println(result)


// $ Call by need:
lazy val lazyValue: Int = {
  println("Evaluating lazyValue")
  10
}

def useLazyValue(x: Int): Unit = {
  println(s"Using lazyValue: $x")
}

useLazyValue(lazyValue)
useLazyValue(lazyValue)


/* Utilisations des deux cas de figures : 
 $ by name: 
 * Delay l'évalutation de l'expression jusqu'à ce qu'on en ait besoin. Performance

 $ by need : 
* Quand on emploie des datastructure infinie ou stream 
* Performances
*/ 


// EX2: 
def myif[A](test: Boolean, onIf: => A, onElse: => A): A = {
	if test then onIf else onElse
}

def myif2[A](test: Boolean)(onIf: => A)(onElse: => A): A = {
	if test then onIf else onElse
}

//Ex 3: 
// ?? 


// Ex 5 : 
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
        
    def takeWhile(cond: A => Boolean): Flux[A] = this match = {
        case Cons(h , t) if cond(h()) => cons(h(), t().takeWhile(cond)) 
        case _ => empty
    }

    def exists(cond: A => Boolean): Boolean = this match {
        case Cons(h, t) if cond(h()) => true else t().exists(cond)
        case _ => false 
    }

    def foldRight[B]( acc: => B)(f:  (A,  => B) => B): B = this match ={
        case Cons(h,t) => f(h(), t().foldRight(acc)(f))
        case _ => acc
    }


    // TP 7 
    def forAll(cond: A => Boolean): Boolean = this match {
        case Cons(h, t) if !cond(h()) => false else t().exists(cond)
        case _ => true 
    }

    



object Flux:
    def cons[A](hd: => A, tl: => Flux[A]): Flux[A] =
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)

    def empty[A]: Flux[A] = Empty

    def apply[A](as: A*): Flux[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail*))