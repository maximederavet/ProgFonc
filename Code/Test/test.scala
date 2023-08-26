object FactorialModule {


	def fac(n: Int): Int = {
		return if(n == 0) 1 else n * fac(n-1)
	}

	def fac2(n: Int): Int = {
		@annotation.tailrec
		def loop(n: Int, acc: Int): Int = {
			if(n == 0) acc
			else loop(n-1, n*acc)
		}
		loop(n, 1)
	}
}

/*
Exerices given by chatgpt
*/
object Sum {

	def sumTo20(): Int = {

		@annotation.tailrec
		def sum(n: Int, acc: Int): Int ={
			if(n == 20) acc+n
			else sum(n+2, acc+n)
		}

		sum(0, 0)
		
	}
}

object Factorial2 {

	def factorial(n: Int): Int ={
		@annotation.tailrec
		def loop(n: Int, acc: Int): Int = {
			if (n == 0) acc
			else loop(n-1, acc*n)
		}

		loop(n, 1);
	}

}


object Convertion {

	val celsiusTemperatures = List(0.0, 20.0, 37.0, 100.0)
	val FahrenheitTemperatures = convert(celsiusTemperatures, celsiusToFahrenheit)
	def celsiusToFahrenheit(degreeC: Double): Double = {
		(degreeC *9/5)+32
	}



	def convert[A,B]( numbers: List[A], convertionRate: A => B ) : List[B] ={
		numbers.map(convertionRate)
	}

	val EurosInt = List(1,4,10,100,25)
	val DollarsList = convert(EurosInt, intEuroToDollars);
	def intEuroToDollars(n: Int): Double = n*1.18; 
	

}


object TP2{
	def findIndexOfLast[A](arr : Array[A], cond: A=>Boolean): Int = {
		def loop(i: Int): Int = {
			if(i <= 0) return -1 //Renvoyer -1 si on arrive à la fin de la liste
			if(cond(arr(i-1))) i-1
			else loop(i-1)

		}

		loop(arr.length)

	}
	def isEven(n: Int ): Boolean = (n%2)==0;
	val numbers = Array.range(1,11,1)

	val result = findIndexOfLast(numbers, isEven)	
}



