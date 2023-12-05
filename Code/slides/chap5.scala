
//Call by value or strict evaluation
//? Une évalutaion au passage dans la fonction
def test(x: Int): Int = 
    println("One")
    val y = x + x
    println("Two")
    y

/*
scala> test({ println("Test") ; 1 })
Test
One
Two
val res0: Int = 2
*/


///Call by name
def test2(x: => Int): Int =
    println("One")
	 //? deux évaluation lors de l'utilisation de x
    val y = x + x
    println("Two")
    y

/*
scala> test2({ println("Test") ; 1 })
One
Test
Test
Two
val res1: Int = 2
*/

//Call by need 1
//? Une seule évaluation de x au passage 
def test3(x: Int): Int = 
    println("One")
    lazy val y = { println("Foo"); x + x }
    println("two")
	 //? y N'est évaluer que ici à son utilisation et non à sa déclaration
    y

/*
scala> test({ println("Test") ; 1 })
Test
One
Two
Foo
val res2: Int = 2
*/

//Call by need 2
//? pareil, x n'est évalué que au passage
def test4(x: Int): Int = 
    println("One")
    lazy val y = { println("Foo"); x + x }
    println("two")
	 //? est évalué à sa première utilisation, mais pas la deuxième
    val a = y+1
	 //Ici la valeur de y est retenue, => on n'a qu'une seule fois foo, malgrè deux utilisations
    val b = y+1
    a + b
    

/*
scala> test({ println("Test") ; 1 })
Test
One
Two
Foo
val res2: Int = 6
*/

//Call by need 3
def test5(x: => Int): Int = 
    println("One")
	 //? Ici les x ne seront pas évalué car y n'est pas évalué à la déclaration
    lazy val y = x + x
    println("two")
	 //? y est évalué uniquement ici, ce qui entraine 2 évaluation de x
    y

/*
scala> test({ println("Test") ; 1 })
One
Two
Test
Test
val res2: Int = 2
*/



///PARTIE 2 EValuation non-stricte

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


object Flux:
    def cons[A](hd: => A, tl: => Flux[A]): Flux[A] =
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    
    def empty[A]: Flux[A] = Empty
    
    def apply[A](as: A*): Flux[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail*))


    def filter(f: A => Boolean): Flux[A] = this match
        case Cons(h, t) if f(h()) => cons(h(), t().filter(f))
        case Cons(_, t) => t().filter(f)
        case _ => empty

    def map[B](f: A => B): Flux[B] = this match
        case Cons(h, t) => cons(f(h()), t().map(f))
        case _ => empty