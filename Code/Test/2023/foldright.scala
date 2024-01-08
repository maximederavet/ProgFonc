enum List[+A]:
	case Nil
	case Cons(head: A, tail: List[A])

	def foldRight[A,B](l: List[A], z: B, f: (A,B)=>B): B = l match
		case Nil => z
		case Cons(h, t) => f(h, foldRight(t, z, f))


	def sumInt(l: List[Int]): Int = 
		foldRight(l, 0, _ + _)

	
	def concat(l: List[String]): String =
		foldRight(l, "", _ ++ _ )

	def count[A](l: List[A]): Int = 
		foldRight(l, 0, (_ :A , x: Int) => x+1)



	val x = Cons(1, Cons(2, Cons(3, Nil)))


