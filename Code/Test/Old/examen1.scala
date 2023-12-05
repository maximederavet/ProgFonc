object Exercice1: 
	def computeIter[A , B](list: List[A], f:(A, A) => B): List[B] = {
		def loop(i: Int, l:List[A]): List[B] = {
			if (l.length < 2) Nil
			else f(l(0), l(1)) :: loop (i+1, l.tail)
		}
		loop(0, list)

		}

	def computeRecursive[I, O](lst: List[I], f: (I, I) => O): List[O] = {
	lst match {
		case Nil | _ :: Nil => Nil
		case x :: y :: tail => f(x, y) :: computeRecursive(y::tail, f)
	}
}


object Exercice2 {
	/*
	def tabulate[A](rows: Int, columns: Int, f:(Int, Int) => A): List[List[A]] = {
		def loop1(i: Int): List[List[A]] ={


			def loop2(j: Int): List[A] = {
				if (j <= columns) List(f(i,j)) :: loop2(j+1)
				else Nil
			}

			 // Looks like List(11, 12, 13)

			if(i <= rows) loop2(1) :: loop1(i+1)
			else Nil

		}

		loop1(1)
	}
	*/

	def tabulate3[A](rows: Int, columns: Int, f: (Int, Int) => A): List[List[A]] = {
		def loop1(i: Int): List[List[A]] = {
			def loop2(j: Int): List[A] = {
				if (j <= columns) f(i, j) :: loop2(j + 1)
				else Nil
			}

			if (i <= rows) loop2(1) :: loop1(i + 1)
			else Nil
		}

		loop1(1)
		}

}


def tabulate2[A](rows: Int, columns: Int, f:(Int, Int) => A): List[List[A]] = {
	(1 to rows).toList.map(i => (1 to columns).toList.map(j => f(i,j)))
}



