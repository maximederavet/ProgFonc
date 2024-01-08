def sumInt(l: List[Int]): Int = 
	l.foldRight(0)(_ + _)

def concat(l: List[String]): String =
	l.foldRight("")(_ ++ _)

def count[A](l: List[A]): Int = 
	l.foldRight(0)( (a,acc) => acc+1)


def maximum(l: List[Int]): Option[Int] = l match
	case Nil => None //! Attention, cas Nil et non l.isEmpty car c'est l que nous somme déjà en train de comparer..
	case _ => Some(l.foldRight(Int.MinValue)((a,acc) => if (a > acc) a else acc))

def inverse[A](l: List[A]): List[A] = 
 	l.foldRight(List[A]())( (a, acc) => acc++List(a)) //! Attention il faut spécifier le type List[A]


def lengthList(l: List[String]): List[Int] = 
	l.foldRight(List[Int]())( (a,acc) => a.size::acc )


def filter[A](l: List[A], p: A => Boolean): List[A] = 
	l.foldRight(List[A]())( (a,acc) => if (p(a)) a::acc else acc  ) 


def map[A, B](l: List[A], f: A => B): List[B] = 
	l.foldRight(List[B]())((a,acc)=> f(a)::acc)

def productInt(l: List[Int]): Option[Int] = l match
	case Nil => None
	case _ => Some(l.foldRight(1)((a,acc) => acc*a))


def safeDivideList(l: List[Int], divisor: Int): List[Either[String, Int]]
	if (divisor == 0) Left("Cannot divide by 0 you idiot !")
	else l.foldRight


val x = List(1,2,3,4,5)

val y = List("1", "2", "3")
val z = List("Salut", "Maxime", "Bonne chance !")