//EX1
def fib1(n: Int): Int = {
   if (n <= 1) n
   else fib1(n - 1) + fib1(n - 2)
}

def fib2(n: Int): Int = {
   @annotation.tailrec
   def loop(n: Int, a: Int, b: Int): Int = {
      if (n == 0) then a
      else loop(n - 1, b, a + b)
   }
   loop(n, 0, 1)
}


//EX2
// for a natural number n, sumInt1(n) = n + n-1 + ... + 0
def sumInt1(n: Int): Int =
   if n == 0 then 0
   else n + sumInt1(n - 1)

// sumInt1 is not tail recursive as the result of the recursive call
// is used in an expression. In other words, the result of the recursive
// call is not returned immediately.
// for a natural number n, sumIn21(n) = n + n-1 + ... + 0
def sumInt2(n: Int): Int =
   // for a natural number n and an integer acc, iter(n, acc) = n + n-1 + ... + acc
   @annotation.tailrec
   def iter(n: Int, acc: Int): Int =
   if n == 0 then acc
   else iter(n - 1, acc + n)
   iter(n, 0)



//EX3
// For an integer x and an natural number n, power1(x,n) = x^n
def power1(x: Int, n: Int): Int =
   if n == 0 then 1
   // else if n % 2 == 0 then power1(x * x, n / 2)
   else x * power1(x, n - 1)
   // power1 is not tail recursive as the result of the recursive call
   // is used in an expression. In other words, the result of the
   // recursive call is not returned immediately.
   // For an integer x and an natural number n, power1(x,n) = x^n

def power2(x: Int, n: Int): Int =
   // For an integer x, a natural number n, and an integer acc,
   // iter(x, n, acc) = x^n * acc
   @annotation.tailrec
   def iter(x: Int, n: Int, acc: Int): Int =
      if n == 0 then acc
      // else if n % 2 == 0 then iter(x * x, n / 2, acc)
      else iter(x, n - 1, acc * x)
   iter(x, n, 1)
   // The addition of the extra recursive step on indirect components '
   // of n turns this function that relies on structural recursion
   // into a function that relies on complete structural recursion.



//EX4
def taken1[A](l: List[A], n: Int): List[A] =
   if n == 0 || l.isEmpty then Nil
   else l.head :: taken1(l.tail, n - 1)


def taken2[A](l: List[A], n: Int): List[A] =
   @annotation.tailrec
   def iter(l: List[A], n: Int, acc: List[A]): List[A] =
      if n == 0 || l.isEmpty then acc
      else iter(l.tail, n - 1, acc ++ List(l.head))

   iter(l, n, Nil)


def taken3[A](l: List[A], n: Int): List[A] =
   @annotation.tailrec
   def iter(l: List[A], n: Int, acc: List[A]): List[A] =
      if n == 0 || l.isEmpty then acc
      else iter(l.tail, n - 1, l.head :: acc)

   iter(l, n, Nil).reverse


//EX5
/*
scala> LBranch(3, LLeaf(1), LBranch(2, LLeaf(1), LLeaf(1)))
val res0: LTree[Int] = LBranch(3,LLeaf(1),LBranch(2,LLeaf(1),LLeaf(1)))
*/


//EX6 
enum LTree[+A]:
   case LLeaf(label: A)
   case LBranch(label: A, left: LTree[A], right: LTree[A])

   /*EX7*/
   def value: A = this match
      case LLeaf(l) => l
      case LBranch(l, _, _) => l
   //Fin ex7

object LTree:
   def compute(t: LTree[_]): Double = t match
      case LLeaf(l: Double) => l
      case LBranch("ADD", left, right) => compute(left) + compute(right)
      case LBranch("SUB", left, right) => compute(left) - compute(right)
      case LBranch("DIV", left, right) => compute(left) / compute(right)
      case LBranch("MUL", left, right) => compute(left) * compute(right)
      
   /*EX7*/
   import Tree._
   def transform(t: Tree[Int], f: (Int,Int) => Int): LTree[Int] =
      t match
         case Leaf(a) => LLeaf(a)
         case Branch(left, right) =>
            val l = transform(left, f)
            val r = transform(right, f)
            LBranch(f(l.value, r.value), l, r
      
      )


import LTree._



val test = LBranch( "MUL",
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

/*
scala>import LTree._
scala>import Tree._
scala>transform(Branch(Branch(Leaf(3),Leaf(5)),Leaf(4)), _ + _)
val res0: LTree[Int] = LBranch(12,LBranch(8,LLeaf(3),LLeaf(5)),LLeaf(4))
*/