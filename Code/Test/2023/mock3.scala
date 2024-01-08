import Flux.*
enum Flux[+A]:
	case Empty
	case Cons(h: () => A, t: () => Flux[A])


	def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
		case Cons(h,t) => f(h(), t().foldRight(acc)(f))
		case _ => acc


	//!
	def bothTrueForAll(p1: A => Boolean, p2: A => Boolean): Boolean = 
		foldRight(true)( (a,acc) => acc && (p1(a) && p2(a)) )
	//!

object Flux:
	def cons[A](hd: => A, tl: => Flux[A]): Flux[A] =
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)

	def empty[A]: Flux[A] = Empty

	def apply[A](as: A*): Flux[A] =
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail*))


