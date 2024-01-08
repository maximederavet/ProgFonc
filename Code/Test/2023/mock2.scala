enum List[+A]: 
	case Cons(head: A, tail: List[A])
	case Nil										//! Pas de parenthèses à Nil


	def g[A, B](l: List[A], t: A => Boolean, f: A => B): List[B] = l match 
		case Nil => Nil
		case Cons(h,tail) => if t(h) then Cons(f(h), g(tail, t, f)) //! Bien remettre Cons à la sortie
									else g(tail, t, f)
	

	//? This function is obviously recursive as it calls itself, on a smaller part of the list, qui se rapproche à chaque fois du cas de base

	//? Cette récursion est structurelle complète, car elle traitera l'intégralité de la structure (la liste)



enum MyList[+A]:
	case MyNil
	case Cons(h: A, t: MyList[A])


	//! Comme on le défini dans l'enum, ceci sont des méthodes => pas de parmaètres
	def head: Option[A] = this match 
		case MyNil => None
		case Cons(h, t) => Some(h)
		
	def tail: Option[MyList[A]] = this match //! Attention au type de retour
		case MyNil => None
		case Cons(h, t) => Some(t)


	object MyList:
		def apply[A](l: A*): MyList[A] =
			if(l.isEmpty) MyNil
			else Cons(l.head, apply(l.tail: _*))


		val myList = MyList(1, 2, 3)
		val headOption = myList.head // Should be Some(1)
		val tailOption = myList.tail // Should be Some(MyList(2, 3))
		val emptyList = MyList()
		val emptyHead = emptyList.head // Should be None
		val emptyTail = emptyList.tail // Should be None





def doremi(x: => Int): Int =
	lazy val a = { println("RE") ; x }
	lazy val b = { println("MI") ; x * x }
	lazy val c = { println("FA") ; x * x }
	if(a == x) a * b else a + c

//! Correct
/*
	doremi({ println("DO") ; 3 })
RE
DO
DO
MI
DO
DO
val res0: Int = 27
*/ 