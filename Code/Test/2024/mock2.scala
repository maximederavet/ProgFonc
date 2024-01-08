/*
def fun1(list: List[Int]): List[(Int, Int, Int)] = 
	if (list.isEmpty) Nil
	else 
		val x = list.head
		(x, x*x, x*x*x)::fun1(list.tail)

	// This implementation is revusrive 
	// This is a structural recursion as it contians only one base case and one appel récursif

def fun2(list: List[Int]): List[(Int, Int, Int)] = list match
	case Nil => Nil
	case h :: t => (h, h*h, h*h*h)::fun2(t)
	//This is also recursive


def g(t: (Int)=>Boolean, f: Int => Int, l: List[Int]): List[Int] = l match
	case Nil => Nil
	case h::tail => if (t(f(h)))  h::g(t,f,tail) else g(t,f,tail)

*/

enum Tree[+A]:
	case Leaf(value: A)
	case Branch(left: Tree[A], right: Tree[A])

enum LTree[+A]:
	case LLeaf(value: A)
	case LBranch(value: A, left: LTree[A], right: LTree[A])

	def label: A = this match
		case LLeaf(v) => v
		case LBranch(v, _, _) => v

import Tree._
import LTree._


//! ça marche !!
def transform[A](t: Tree[A], f:(A,A)=>A):LTree[A] = t match
	case Leaf(v) => LLeaf(v)
	case Branch(l , r) => LBranch( f(transform(l,f).label, transform(r,f).label) , transform(l,f), transform(r, f))




enum MyList[+A]:
	case MyNil
	case Cons(h: A, t: MyList[A])
	
	def head: Option[A] = this match
		case MyNil => None
		case Cons(h,t) => Some(h)
	

	def tail: Option[MyList[A]] = this match 
		case MyNil => None
		case Cons(h,t) => Some(t)
		
object MyList:
	def apply[A](l: A*): MyList[A] =
		if(l.isEmpty) MyNil
		else Cons(l.head, apply(l.tail: _*))

/* Demonstrate its use */
import MyList.*
val l = MyList(Cons(1,MyNil))
val l2 = MyList(Cons(1,Cons(2,Cons(3,MyNil))))

/*
scala> apply(l2)
val res3: MyList[MyList[MyList[Int]]] = Cons(Cons(Cons(1,Cons(2,Cons(3,MyNil))),MyNil),MyNil)
*/

def doremi(x: => Int): Int =
	lazy val a = { println("RE") ; x }
	lazy val b = { println("MI") ; x * x }
	lazy val c = { println("FA") ; x * x }
	if(a == x) a * b else a + c

/*
doremi({ println("DO") ; 3 })
RE
DO
DO
MI
DO
DO
val res4: Int = 27
//! Correct 
*/