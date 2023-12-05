enum Tree[+A]:
    case Leaf(value: A)
    case Branch(left: Tree[A], right: Tree[A])

object Tree:
    def fold[A, B](tree: Tree[A])(leafFunc: A => B)(branchFunc: (B, B) => B): B = tree match
        case Leaf(value) => leafFunc(value)
        case Branch(left, right) => branchFunc(fold(left)(leafFunc)(branchFunc), fold(right)(leafFunc)(branchFunc))


def mul(tree: Tree[Int]): Int = {
    Tree.fold(tree)(value => value)(_*_)
}

def f1(n: Int):Double ={
	if (n == 0) return 1.0 
	else return f1(n - 1) / 2.0
}

@annotation.tailrec
def f2(n: Int, acc: Double ): Double = {
	if (n == 0) return acc
	return f2(n-1, acc/2) 
}

case class Transaction(account : String, amount: Double)

def cmptBalances(initBal: Double, transactions: List[Transaction]): List[Double] = {
	def cmptBalancesRec(balance: Double, remainingTransactions: List[Transaction], acc: List[Double] ): List[Double] = {
		remainingTransactions match {
			case Nil => acc	
			case Transaction(_, amount) :: rest => 
				cmptBalancesRec(balance + amount, rest, balance + amount :: acc) 
		}
			
	}

	cmptBalancesRec(initBal, transactions, List(initBal)).reverse
}