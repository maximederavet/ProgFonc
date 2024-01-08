case class Pokemon(name: String, hp: Int, att: Int, deff: Int, spatt: Int, spdef: Int, speed: Int)

val gen1 = List( Pokemon("Bulbasaur",45,49,49,65,65,45), Pokemon("Ivysaur",60,62,63,80,80,60),
	Pokemon("Venusaur",80,82,83,100,100,80), Pokemon("Charmander",39,52,43,60,50,65),
	Pokemon("Charmeleon",58,64,58,80,65,80), Pokemon("Charizard",78,84,78,109,85,100),
	Pokemon("Squirtle",44,48,65,50,64,43), Pokemon("Wartortle",59,63,80,65,80,58),
	Pokemon("Blastoise",79,83,100,85,105,78))

def isBulky(p: Pokemon): Boolean = 
	(p.deff > p.att)&&(p.spdef > p.spatt)

def procees[B](lp: List[Pokemon], f: Pokemon => B): List[B] = 
	lp.map(f)

// val meanAtt = gen1.sum(if( _.speed >70) _.att )/ ??? on doit faire en deux passages 


//Exercice 2 

def f1(n: Int): Double =
	if(n==0) 1
	else f1(n-1)/2
	


def f2(n: Int): Double = {
	@annotation.tailrec
	def helper(i: Int, acc: Double): Double = 
		if (i <= 0) acc
		else helper(i-1, acc/2)

	helper(n, 1)
}

/* Est-ce une récursivité structurelle ? 
 * - Oui, il y a un cas de base
 * - Chaque appel se fait sur une sous-partie du précédent
 * - Chaque appel se rapproche du cas de base
 */

// QUEstion 3 
enum Tree[+A]:
	case Leaf(value: A)
	case Branch(left: Tree[A], right: Tree[A])

	val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

	def fold[A,B](t: Tree[A], f: A => B, g: (B,B) => B ): B = t match //! Renvoie un type B et non Tree[B]
		case Leaf(v) => f(v)
		case Branch(r, l) => g(fold(r,f,g), fold(l,f,g))

	val res1 = fold(tree, (x: Int) => x, (x: Int,y: Int)=> (x+y) )  //!


//Question 4
// Exception Handling


enum Either[+E, +A]:
	case Left(get: E)
	case Right(get: A)

def max(list: List[Int]): Either[String, Int] = 
	if (list.isEmpty) Left("List is empty")
	else Right(list.max)