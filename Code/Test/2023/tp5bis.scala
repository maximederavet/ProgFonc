/*
 * Exception Handling : TP5
 */
import Option._

 // EXercice 1 
enum Tree[+A]:
	case Leaf(label: A)
	case Branch(left: Tree[A], right: Tree[A])

object Tree:
	extension (tree: Tree[Int])

		//! Remove the parameter tree of the functions as it is handled uphere 
		def biggestInt: Int = tree match
			// Cette fonction est totale car elle retournera tjrs qqch pour tous les Int. 
			case Leaf(v) => v
			case Branch(l , r) => l.biggestInt.max(r.biggestInt) //! Attention, il n'y plus de paramètres aux fonctions
																				  //! Elles sont transformées en méthodes



		def firstPositive: Option[Int] = tree match //! Attention au type de retour Option[Int] et pas Int

			case Leaf(v) => if v>0 then Some(v) else None //! Attention ici , si v plus grand que 0 alors la valeur, sinon rien

			case Branch(l , r) =>  l.firstPositive.orElse(r.firstPositive)  //! Pareil qu'au dessus, soit l si il y a qqch, orElse r
			
			// Not total function since in the instance of an all negative tree, there could be no value to return
			// Not total = Partial function => Need Excpetion Handling

	extension (tree: Tree[String])

		def esrever: Tree[String] = tree match 
			case Leaf(s) => Leaf(s.reverse)
			case Branch(l , r) => Branch(l.esrever, r.esrever)
			
			// Cette fonction est totale car toutes les string peuvent être reversed



import Tree._
val ti = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Leaf(4)))
val ts = Branch(Branch(Leaf("01"),Leaf("02")),Branch(Leaf("03"),Leaf("04")))




// Either 
enum Either[+E, +A]:
	case Left(get: E)
	case Right(get: A)

	def toList: List[A] = this match
		case Right(a) => List(a)
		case Left(_) => List()

	def map[B](f: A => B): Either[E, B] = this match
		case Right(a) => Right(f(a)) //! Modifie juste la valeur de a, il doit être remis dans un right ensuite
											  //! f: A => B !!!
		case Left(e) => Left(e)

	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match 
		case Right(a) => f(a)  //! f retourne ici déjà un type Either, nous ne devons pas le remettre dans un Right
									  //! f: f: A => Either[EE, B] !!!
		case Left(e) => Left(e)

	def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match
		case Left(_) => alt
		case Right(a) => Right(a)

	def getOrElse[B >: A](default: => B): B = this match
		case Left(_) => default
		case Right(a) => A

	def toOption: Option[A] = this match
		case Left(_) => None
		case Right(a) => Some(a)


import Either._

def lift[A,B](f: A =>B): Either[_,A] => Either[_,B] = _.map(f)


//! Either en pratique
def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
	try Right(x/y)
	catch case e: Exception => Left(e) // Catching runtime exception

def safeFib(n: Int): Either[String, Int] = 
	@annotation.tailrec
	def helper(n: Int, prev: Int, acc: Int): Either[String, Int] = 
		if (n < 0) Left("Argument is negative")
		if (n == 0) Right(acc)
		else helper(n-1, acc, acc + prev)
	helper(n, 0, 1)

