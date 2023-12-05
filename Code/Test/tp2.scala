def findIndexOfLast[A](arr: Array[A], cond: A => Boolean): Int = {
	
	

	def aux(index: Int): Int = {
		if (index < 0) -1
		else if (cond(arr(index))) index+1
		else aux(index-1)
	}

	return aux(arr.length-1)

}

val  array1 = Array(1,2,3,4,5,6,7,8,9,10)
val res1 = findIndexOfLast(array1, _ % 3 == 0)


def isSorted[A](as: Array[A], comp: (A,A) => Boolean): Boolean = {

	def loop(n: Int): Boolean =  {
		if (n >= as.length - 1) true
		else if (comp(as(n), as(n+1))) loop(n+1)
		else false
	}
	loop(0)
} 
val res2 = isSorted(array1, (x,y) => x<y)


def counIf[A](arr: Array[A], cond: (A)=> Boolean): Int = {

	def loop(ind: Int, acc: Int): Int = {
		if (ind >= arr.length) acc
		else if (cond(arr(ind))) loop(ind + 1, acc + 1)
		else loop(ind + 1, acc)
	}
	loop(0, 0)
}

val res3 = counIf(array1, _ % 2 == 0)


def counIf2[A](arr: Array[A], cond: (A)=> Boolean): Int = {
	arr.filter(cond).size
}

def counIf3[A](arr: Array[A], cond: (A)=> Boolean): Int = {
	arr.count(cond)
}


