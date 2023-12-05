
enum List[+A]:
	case Nil
	case Cons(head: A, tail: List[A])

object List:
	def sum(ints: List[Int]): Int = ints match 
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)

	/*
	def reverseList[A](list: List[A]): List[A] = list match
		case Nil => Nil
		case Cons(h, t) => r everseList(t) + h
	*/

	def product(list: List[Double]): Double = list match {
		case Nil => 1.0
		case Cons(0.0 , _) => 0.0
		case Cons(x, xs ) => x * product(xs)
	}

	
	def apply[A](l: A*): List[A] = 
		if(l.isEmpty) Nil
		else Cons(l.head, apply(l.tail: _*))

	def map[A, B](list: List[A], f: A=> B): List[B] = list match {
		case Nil => Nil
		case Cons(h,t) => Cons( f(h), map(t, f))
	}



enum Tree[+A]:
	case Leaf(value: A)
	case Branch(left: Tree[A], right: Tree[A])

	def size: Int = this match{
		case Leaf(_) => 1
		case Branch(l , r) => 1 + l.size + r.size
	}

	def depth: Int = this match{
		case Leaf(_) => 1
		case Branch(l , r) => 1 + l.depth.max(r.depth)
	}

	def map[B](f: A => B): Tree[B] = this match {
		case Leaf(a) => Leaf(f(a))
		case Branch(l , r) => Branch(l.map(f), r.map(f))
	}
    

