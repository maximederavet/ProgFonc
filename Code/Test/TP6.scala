/*
 * What is Call by name ? 
 - Call by Name (Appel par Nom) : Cette stratégie retarde l'évaluation d'une
  expression jusqu'à ce que sa valeur soit nécessaire. Chaque fois que 
  l'expression est accédée, elle est réévaluée. En Scala, cela se fait en 
  utilisant => (notation fléchée). C'est utile pour améliorer les performances 
  lorsque l'argument n'est pas toujours utilisé dans la fonction.
 */

 /* What is call by Need ? 
 * - Call by Need (Appel selon le Besoin) : Aussi connu sous le nom d'évaluation 
 * paresseuse, cette stratégie est similaire à l'appel par nom, mais avec une 
 * optimisation supplémentaire. L'expression est évaluée seulement la première 
 * fois qu'elle est nécessaire, et le résultat est mis en cache pour les utilisations 
 * ultérieures. Cela évite la surcharge des évaluations répétées. En Scala, 
 * cela peut être implémenté en utilisant lazy val.
 */


// How do we call by need or by name ? 

									//! : =>
def exampleCallByName(condition: => Boolean): Unit = {
  if (condition) {
    println("Condition is true")
  } else {
    println("Condition is false")
  }
}

//! lazy val
lazy val expensiveComputation: Int = {
  println("Computing...")
  // Some expensive computation
  42 // Assuming this is the result of the computation
}



/// Exercice 1 : Demonstrating Behavior and explanation : 
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


/// EXERCICE 2 : myIf

def myIf[A](test: Boolean, onIf: => A, onElse:  => A ): A = 
	if test then onIf else onElse

//! Currying
def myIf2[A](test: Boolean)(onIf: => A)(onElse: => A): A = 
	if test then onIf else onElse

/*
scala> myif(3 > 5, 2 / 0, 5)
val res0: Int = 5
scala> myif2(3 > 5)(2 / 0)(5)
val res1: Int = 5
*/

/*
 * Motivation ? Nous pouvons utiliser le currying pour fixer certains paramètres 
 * Nous créons ainsi une application partielle
 */

val partial = myIf2(2 > 1) _ 
val result = partial("True Case")("False Case")



//Exercice 3 : myIfElse

//EX3
def myifelseifelse[A](t1: Boolean, i: => A, t2: => Boolean, ei: => A, e: => A): A =
    myif(t1, i, myif(t2, ei, e)) 

	//TODO understand this shit

//EX5 

//! The main difference between Flux and List is that the evaluation in Flux is Lazy
//! Which means that the elements are only evaluated once they are accessed
//! As for lists, the elements are evaluated at the declaration

import Flux.*
enum Flux[+A]:
	case Empty
	case Cons(h: () => A, t: () => Flux[A])
	//! Notice how the declaration is () => A and not A

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


	def takeWhile(p: A => Boolean): Flux[A] = ???
	def exists(p: A => Boolean): Boolean = ???
	def foldRight[B](acc: => B)(f: (A, => B) => B): B = ???

	
object Flux:
	def cons[A](hd: => A, tl: => Flux[A]): Flux[A] =
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)

	def empty[A]: Flux[A] = Empty

	def apply[A](as: A*): Flux[A] =
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail*))

