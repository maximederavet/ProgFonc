def sumInt(l: List[Int]): Int = 
	l.foldRight(0)( _ + _)

def concat(l: List[String]): String =
	l.foldRight("") (_ ++ _ )

def count[A](l: List[A]): Int = 
	l.foldRight( 0)( (_ :A , x: Int) => x+1)

def maximum(l: List[Int]): Option[Int] = l match
	case Nil => None
	case _ => Some( l.foldRight(0)((x: Int, max: Int) => if x > max then x else max ) )

def inverse[A](l: List[A]): List[A] = 
	l.foldRight(List[A]())((h,t) => t :+ h )

def lengthList(l: List[String]): List[Int] = 
	l.foldRight(List[Int]())( (h,t) => h.size :: t )

def filter[A](l: List[A], p: A => Boolean): List[A] = 
	l.foldRight(List[A]()) ((h, t) => if p(h) then h::t else t )

def map[A, B](l: List[A], f: A => B): List[B] = 
	l.foldRight(List[B]())( (h,t) => f(h) :: t  )

def productInt(l: List[Int]): Int = 
	l.foldRight(1)(_ * _)

def isEven(l: List[Int]): Boolean = 
	l.foldRight(true)(  (a, acc) => acc && (a%2 ==0))




val x = List(1,2,3,4,5)

val y = List("1", "2", "3")