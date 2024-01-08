/* Question 2 2022 */
def f1(n: Int): Double = 
	if (n == 0) 1
	else f1(n-1)/2

/* Ceci est une récursivité structurelle car nous n'avons qu'un seul cas de base et un seul appel récursif, qui s'appuie sur des éléments directs */


def f2(n: Int): Double = 

	@annotation.tailrec
	def loop(x: Double, i: Int): Double = 
		if (i==0) x
		else loop(x/2, i-1)

	loop(1,n)

/* ok good */


case class Transaction(account: Int, amount: Double)
val trans = List(Transaction(1,-10),Transaction(2,30),Transaction(2,-40))

def computebalances(trans: List[Transaction], initBal: Double): List[Double] =
	if (trans.isEmpty) Nil
	else { val act = initBal + trans.head.amount ; act::computebalances(trans.tail, initBal+trans.head.amount);}

	/* This is a recursive function as it calls a sub part of it */
	/* this recursion is strcutural. There is only one base case and one recursvie call on the structure. The whole structure will be treated */


import Either._

def max(l: List[Int]): Either[String, Int] = 
	if (l.isEmpty) Left("List is impty")	
	else if (l.tail.isEmpty) Right(l.head)
	else Right(l.head.max(max(l.tail).getOrElse(Int.MinValue)))

val l = List()

/*
enum Tree[+A]: 
	case Leaf(value: A)
	case Branch(left: Tree[A], right: Tree[A])

	def fold[B](leafFunc: A => B)(branchFunc: (B, B) => B): B = this match
		case Leaf(value) => leafFunc(value)
		case Branch(left, right) => branchFunc(left.fold(leafFunc)(branchFunc), right.fold(leafFunc)(branchFunc))

	/*
	def mul[A]:A = this match
		case Leaf(v) => v
		case Branch(l,r) => (l.mul*r.mul)
	*/

	def mul = this.fold(v => v)(_ * _)

*/


case class Pokemon(name: String, hp: Int, att: Int, deff: Int, spatt: Int, spdef: Int, speed: Int)

val gen1 = List( Pokemon("Bulbasaur",45,49,49,65,65,45), Pokemon("Ivysaur",60,62,63,80,80,60),
	Pokemon("Venusaur",80,82,83,100,100,80), Pokemon("Charmander",39,52,43,60,50,65),
	Pokemon("Charmeleon",58,64,58,80,65,80), Pokemon("Charizard",78,84,78,109,85,100),
	Pokemon("Squirtle",44,48,65,50,64,43), Pokemon("Wartortle",59,63,80,65,80,58),
	Pokemon("Blastoise",79,83,100,85,105,78))

def isBulky(p: Pokemon): Boolean = 
	(p.deff > p.att && p.spdef > p.spatt)

def process[B](l: List[Pokemon], f: Pokemon => B ): List[B] = 
	l.map(f)


val att = gen1.foldRight(0)((p, acc) => if (p.speed > 70) acc + p.att else acc+ 0 )


val (totalAtt, count) = gen1.foldRight((0, 0)) { (p, acc) =>
  if (p.speed > 70) (acc._1 + p.att, acc._2 + 1)
  else acc
}

val meanAtt2 = totalAtt/count
