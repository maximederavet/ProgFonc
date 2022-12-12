//EX3
/*
scala> findIndexOfLast(Array(1,2,3,4,2,3,2), _==3)
val res0: Int = 5
*/
def findIndexOfLast[A](arr: Array[A], cond: A => Boolean): Int =
  @annotation.tailrec
  def loop(n: Int): Int =
    if(n < 0) -1
    else if (cond(arr(n))) n
    else loop(n - 1)
  loop(arr.size - 1)


//EX4
/*
scala> isSorted(Array(1,2,3,4), (x,y)=> x<y)
val res3: Boolean = true
*/
def isSorted[A](as: Array[A], comp: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def helper(n: Int): Boolean = {
    if (n >= as.length - 1) then true
    else if (!comp(as(n), as(n + 1))) then false
    else helper(n + 1)
  }
  helper(0)
}


//EX5
def countIf[A](arr: Array[A], cond: A => Boolean): Int =
  @annotation.tailrec
  def loop(n: Int, acc: Int): Int =
    if(n >= arr.length) acc
    else loop(n + 1, if(cond(arr(n))) acc + 1 else acc + 0)
  loop(0, 0)

def countIf2[A](arr: Array[A], cond: A => Boolean): Int =
  arr.filter(cond).size
def countIf3[A](arr: Array[A], cond: A => Boolean): Int =
  arr.count(cond)
