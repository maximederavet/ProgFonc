enum List[+A]:
	case Nil
	case Cons(head: A, tail: List[A])

object List:

	def foldRight[A,B](l: List[A], z: B, f: (A,B)=>B): B = l match
		case Nil => z
		case Cons(h, t) => f(h, foldRight(t, z, f))

	def append[A](l1: List[A], l2: List[A]): List[A] = l1 match
		case Nil => l2
		case Cons(h, t) => Cons(h, append(t, l2))
	
	def apply[A](l: A*): List[A] = {
		if(l.isEmpty) Nil
		else Cons(l.head, apply(l.tail: _*))
	}

	def concat[A](l: List[List[A]]): List[A] = l match 
		case Nil => Nil
		case Cons(h, t) => append(h, concat(t)) // On fusionne t à h

	def map[A,B](l: List[A], f: A=> B): List[B] = l match
		case Nil => Nil
		case Cons(h,t) => Cons(f(h) , map(t, f)) 

	def fmap[A, B](l: List[A], f: A => List[B]): List[B] = l match
		case Nil => Nil
		case Cons(h, t) => append(f(h), fmap(t, f))

	/*
	def purge[A](l: List[A], cond: A => Boolean): List[A] = { // Ne fait pas partie de l'exercice, supprime tous les éléments qui ne respectent pas une condition
		if (l.isEmpty) Nil
		else if (cond(l.head)) Cons(l.head, purge(l.tail, cond))
		else purge(l.tail, cond)
	}
	*/

	def dropWhile[A](l: List[A], cond: A => Boolean): List[A] = l match // T'en qu'on respecte une condition, on passe au suivant. Dès que plus respecté, renvoyer le reste de la liste
		case Cons(h, t) if(cond(h)) => dropWhile(t, cond)
		case _ => l

	def length[A](l: List[A]): Int = l match
		case Nil => 0
		case Cons(_, t) => 1 + length(t)

		// Exercice 2 
	val x = List(1,2,3,4)

	val rs1 = map(x, _ + 3)
	val rs2 = fmap(x, (x: Int) => List(x + 3))
	val rs3 = dropWhile(x, _ <= 3)
	val rs4 = concat(List(x, x))



	val rs5 = foldRight(List(1,2,3,4), 0 , (a, b) => a + b) // SUm a list using foldright

	def concat2[A](l: List[List[A]]): List[A] = 
		foldRight(l, Nil: List[A], append)

	def map2[A, B](l: List, f: A => B ): List[B] = 
		foldRight(l, Nil: List[B], (h,t) => Cons(f(h), t))

	def fmap2[A, B](l: List[A], f: A => List[B]): List[B] = 
		foldRight(l, Nil: List[B], (h, t) => Cons(f(h), t))

	def append2[A](l1: List[A], l2: List[A]): List[A] = 
		foldRight(l1, l2, (l, acc) => Cons(l, acc) )
	
enum Tree[+A]: 
	case Leaf(value: A)
	case Branch(left: Tree[A], right: Tree[A])

	def size: Int = this match
		case Leaf(_) => 1
		case Branch(l, r) => 1 + size(r) + size(l)

	def depth: Int = this match 
		case Leaf(_) => 1
		case Branch(l , r) => 1 + l.depth.max(l.r.depth)

	def map[A, B](f: A => B): Tree[B] = this match 
		case Leaf(v) => Leaf(f(v))
		case Branch(l , r) => Branch(l.map(f), r.map(f))







	

		



	


	