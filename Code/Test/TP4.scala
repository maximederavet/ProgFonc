enum List[+A]:
	case Nil
	case Cons(head: A, tail: List[A])

object Functions:
	def fib1(n: Int): Int = {
		
		@annotation.tailrec
		def loop(i: Int, acc1: Int, acc2: Int): Int ={
			if (i<n) loop(i+1, acc2, acc1+acc2)
			else acc1+acc2
		}
		
		if (n==0) return 0
		else loop(1, 1, 0)
	}


	//correction plus efficace
	def fib2(n: Int): Int = {

		@annotation.tailrec
		def loop(n: Int, acc1: Int, acc2: Int ): Int = {
			if (n == 0) acc1
			else loop(n-1, acc2, acc1+acc2)
		}

		loop(n, 0, 1)
	}
	
	def power(x: Double, n: Int): Double ={
		@annotation.tailrec
		def loop(acc1: Double, n: Int): Double ={
			if (n == 0) acc1
			else loop(x * acc1 , n-1)
		}

		loop(1, n)
			
	}


