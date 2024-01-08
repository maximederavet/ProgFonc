def fib1(n: Int): Int = 
	
	@annotation.tailrec
	def helper(prev: Int, act: Int, i: Int): Int = 
		if (i >= n) act + prev
		else helper(act, act + prev, i+1)
	
	if ( n <= 1 ) n
	else helper(0, 1, 2)

	// The definition here is tail recursive as we do not have to remonter la pile. Lorsqu'on arrive à un i statisfaisant, on renvoit les nombres calculés

def fib2(n: Int ): Int = 
	// Not tail revursive
	// To do probabably
	0 


//Exercice 2
def sumInt1(n: Int): Int = 

	@annotation.tailrec
	def helper(i: Int, acc: Int): Int = 
		if (i == 0) acc
		else helper(i-1, acc+i)

	helper(n, 0)

// Not tailrec
def sumInt2(n: Int): Int = 
	if (n == 0) 0
	else n + sumInt2(n-1)


// Exercice 4 
// Not tailrec
def taken1[A](list: List[A], n: Int): List[A] = 
	if (list.isEmpty || (n == 0)) Nil
	else list.head :: taken1(list.tail, n-1)
	//else list.head +: taken1(list.tail, n-1)  //! Les deux fonctionnent, mais +: marche aussi pour les vecteur etc.. alors que :: est spécifique aux listes

def taken2[A](list: List[A], n: Int): List[A] = 

	@annotation.tailrec
	def helper(acc: List[A], i: Int, l: List[A]): List[A] = 
		if (l.isEmpty || (i == 0)) acc
		else helper(acc :+ l.head, i-1, l.tail )   //! Ne peut que être :+ pour ajouter à la fin
		// else helper(acc ++ List(l.head), i-1, l.tail ) //! Exemple du prof. Avec ++ on doit spécifier à nouveau list, car l.head est uniquement l'élément
	helper(List(), n, list)

enum LTree[+A]:
	case LLeaf(label: A)
	case LBranch(label: A, left: LTree[A], right: LTree[A])

object LTree:
	def compute(t: LTree[_]): Double = t match
		case LLeaf(l: Double) => l
		case LBranch("ADD", left, right) => compute(left) + compute(right)
		case LBranch("SUB", left, right) => compute(left) - compute(right)
		case LBranch("DIV", left, right) => compute(left) / compute(right)
		case LBranch("MUL", left, right) => compute(left) * compute(right)
		case _ => 0.0  // Default case to handle other types or unrecognized labels

import LTree._

val test = LBranch("MUL",
LBranch("ADD",
	LBranch("ADD",
		LLeaf(3.0),
		LLeaf(5.0)),
	LBranch("SUB",
		LLeaf(3.0),
		LLeaf(4.0))),
LBranch("DIV",
	LLeaf(3.0),
	LLeaf(2.0)))

val test2 = compute(test)


// ENTRAINEMENT à FOLD

val numbers = List(1, 2, 3, 4, 5)
val sum = numbers.foldLeft(0)(_ + _) // 0 est notre base, _+_ est notre fonction 

