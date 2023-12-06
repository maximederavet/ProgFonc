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

	//! Notice that we use h() and t() to access their values (and not h and t )
	def toList: List[A] = this match
		case Cons(h,t) => h() :: t().toList
		case Empty => Nil

	def take(n: Int): Flux[A] = this match //Takes the first n elements from a list
		case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))   //! We use cons defined in the companion object (and not Cons)
		case Cons(h, _) if n == 1 => cons(h(), empty)
		case _ => empty				//! We use empty, defined in the companion object  (and not Empty)

	def filter(f: A => Boolean): Flux[A] = this match //Filter values respecting a condition
		case Cons(h, t) if f(h()) => cons(h(), t().filter(f))
		case Cons(_, t) => t().filter(f)
		case _ => empty

	def map[B](f: A => B): Flux[B] = this match //Applies f to elements
		case Cons(h, t) => cons(f(h()), t().map(f)) 
		case _ => empty


	def takeWhile(p: A => Boolean): Flux[A] = this match
		case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
		case _ => empty


	def exists(p: A => Boolean): Boolean = this match
		case Cons(h, t) if p(h()) => true
		case Cons(h, t) if !p(h()) => t().exists(p)
		case _ => false //! Attention au type de retour (pas empty bêtement)
		

	def foldRight[B](z: => B)(op: (A, => B) => B): B = this match {
		case Cons(h, t) => op(h(), t().foldRight(z)(op))
		case _ => z
	}

	def forAll(p: A => Boolean): Boolean = this match
		case Cons(h, t) if p(h()) => t().forAll(p)
		case Cons(h, t) if !p(h()) => false
		case _ => true



	def takeWhile2(p: A => Boolean): Flux[A] =  
		foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else empty)
		//! type                        //! cons et non ::

	def headOption2: Option[A] = 
		foldRight[Option[A]](None)((a, _) => Some(a))

	
	
/*
	def map2

	def filter2

	def append2

	def flatMap2[B](f: A => Flux[B]): Flux[B] = 
		foldRight(empty: Flux[B], (a, acc) => f(a).append(acc))

	def append[B]
*/

	
object Flux:
	def cons[A](hd: => A, tl: => Flux[A]): Flux[A] =
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)

	def empty[A]: Flux[A] = Empty

	def apply[A](as: A*): Flux[A] =
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail*))

