/*
 * Exception Handling : TP5
 */
import Option._

 // EXercice 1 
enum Tree[+A]:
	case Leaf(label: A)
	case Branch(left: Tree[A], right: Tree[A])

object Tree:
	def size[A](t: Tree[A]): Int = t match
	case Leaf(_) => 1
	case Branch(l, r) => size(l) + size(r) + 1

	def biggestInt(tree: Tree[Int]): Int = tree match
		// Cette fonction est totale car elle retournera tjrs qqch pour tous les Int. 
		case Leaf(v) => v
		case Branch(l , r) => biggestInt(l).max(biggestInt(r))



	def firstPositive(tree: Tree[Int]): Option[Int] = tree match //! Attention au type de retour Option[Int] et pas Int

		case Leaf(v) => if v>0 then Some(v) else None //! Attention ici , si v plus grand que 0 alors la valeur, sinon rien

		case Branch(l , r) =>  firstPositive(l) orElse firstPositive(r)  //! Pareil qu'au dessus, soit l si il y a qqch, orElse r
		
		// Not total function since in the instance of an all negative tree, there could be no value to return
		// Not total = Partial function => Need Excpetion Handling





import Tree._
val ti = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Leaf(4)))
val ts = Branch(Branch(Leaf("01"),Leaf("02")),Branch(Leaf("03"),Leaf("04")))

