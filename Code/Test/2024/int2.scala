case class Transaction(account: String, amount: Double)

val transactions = List(Transaction("1",-10),Transaction("2",30),Transaction("2",-40))



def adjustCurrency2(trans: List[Transaction], f: Double => Double): List[Transaction] = trans match
	case Nil => Nil
	case h::tail => Transaction(h.account, f(h.amount))::adjustCurrency2(tail, f)

// Cette version est récursive structurelle, car elle s'appelle elle-même 1 seule fois, avec un seul cas de base, sur des éléments directs de la listes; et la liste décroit en taille, garantissant ainsi la terminaison
	

def adjustCurrency(trans: List[Transaction], f: Double => Double): List[Transaction] = 
	trans.map(t => Transaction(t.account, f(t.amount))) //!

def adjustCurrency3(trans: List[Transaction], f: Double => Double): List[Transaction] = 
	trans.foldRight( List[Transaction]() )( (a: Transaction,acc: List[Transaction]) => Transaction(a.account, f(a.amount)) :: acc ) //! Acc de type List[Transaction] ()




/* Question 2 */
// Nous pouvons utiliser le currying pour n'entrer qu'un seul argument, la taux de conversion, et fixer la liste de transactions utilisée

//! Important
def adjustCurrencyCurr(trans: List[Transaction])(f: Double => Double): List[Transaction] = 
	trans.foldRight( List[Transaction]() )( (a: Transaction,acc: List[Transaction]) => Transaction(a.account, f(a.amount)) :: acc ) //! Acc de type List[Transaction] ()

def adjustTransactions = adjustCurrencyCurr(transactions) _


/* Question 4*/ 
enum MyList[+A]:
	case MyNil
	case Cons(h: A, t: MyList[A])

	def tail: Option[MyList[A]] = this match //! Attention tail renvoie des MyList[A] !!!
		case MyNil => None
		case Cons(h,t) => Some(t)

	def head: Option[A] = this match
		case MyNil => None
		case Cons(h,t) => Some(h)

/* Question 5*/
def mul(l: List[Int]): Int = {
  @annotation.tailrec      //! Changed here 
  def iter(l: List[Int], acc: => Int): Int = l match {
    case Nil => acc
    case 0 :: _ => 0
    case h :: t => iter(t, h * acc)
  }
  iter(l, 1)
}

/*
La technique utilisée s'appelle le call by name. Nous n'avaluons le paramètre que l'orsqu'il est utilisé dans des calculs
En l'occurence, ici lors d'un calcul de produit, il est inutile de calculer toute l'expression si on rencontre un 0 
*/


