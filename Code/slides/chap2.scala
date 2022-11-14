//Le code de la factorielle du chap1 n'est pas une récursion terminale. Transformons-le
object FactorialModule:
    def fac(n: Int): Int = {

        @annotation.tailrec  //Si une fonction est de récursivité terminale, le développeur toi le préciser pour que le compilateur puisse optimiser le code
        def iter(n: Int, acc: Int): Int =
            if(n == 0) acc
            else iter(n-1, n*acc)
    
        iter(n, 1)
    }

    private def formatFac(n: Int) = { //Private n'est pas important pour ce cours-ci 
        val message: String = "The factorial of %d is %d"
        message.format(n, fac(n))
    }

    def main(args: Array[String]): Unit =
        println(formatFac(5))

end FactorialModule

object MathModule: 

    private def helper(n: Int, acc:Int, op: (Int, Int) => Int): Int = //op est une fonction binaire
        @annotation.tailrec
        def h(n: Int, acc: Int): Int =
            if(n == 0) acc
            else h(n-1, op(n, acc))
        
        h(n, acc)

    def fac(n: Int): Int = helper(n, 1, _ * _) // _*_ est du sucre synthaxique pour (a, b) => (a * b)

    def trianglenumber(n: Int): Int = helper(n, 0, _+_)

    def main(args: Array[String]): Unit = 
        val l = List(0,1,2,3,4,5)
        println(l.map(fac))
        println(l.map(trianglenumber))  //map permet d'appliquer la fonction à tous les éléments de la liste


object Polymorphic: 

    def finIdexOfFirstString(arr: Array[String], key: String): Int = 
        @annotation.tailrec
        def loop(n: Int): Int =
            if(n >= arr.length) -1
            else if (arr(n) == key) n
            else loop(n + 1)

        loop(0)

    def finIdexOfFirstInt(arr: Array[Int], key: Int): Int = 
        @annotation.tailrec
        def loop(n: Int): Int =
            if(n >= arr.length) -1
            else if (arr(n) == key) n
            else loop(n + 1)
            
        loop(0)
        
    // Selon le type de données en entrées, on doit avoir deux fonctions
    // alors que leur corps est le même
    // Solution : Fonction Polymorphique. Elles acceptent plusieurs types (au moins deux)
    // le type devra être spécifié en entrée

    def finIdexOfFirstPoly[A](arr: Array[A], key: A): Int = 
        @annotation.tailrec
        def loop(n: Int): Int =
            if(n >= arr.length) -1
            else if (arr(n) == key) n
            else loop(n + 1)
            
        loop(0)


    /* Redéfinissons findIndexOfFirst de manière à pouvoir rentrer
        une fonction lambda. Ici on va rentrer une condition
        rappel: une fonction lambda est une fonction anonyme
    */
    def finIdexOfFirstLambda[A](arr: Array[A], cond: A => Boolean): Int = 
        @annotation.tailrec
        def loop(n: Int): Int =
            if(n >= arr.length) -1
            else if (cond(arr(n))) n
            else loop(n + 1)
            
        loop(0)
    /*
    scala> HOModule.findIndexOfFirstLambda(Array(1, 2, 1, 2, 3), (x) => { x == 3 })
    val res1: Int = 4
    lorsque la condition x == 3, est respectée, la fonction renvoie l'index n
    */

def partial[A, B, C](a: A, f: (A, B) => C) : B => C =
    (b) => f(a, b)

/*
scala> val plus = (x: Int, y:Int) => x + y
val plus: (Int, Int) => Int = Lambda$1696/0x000000080115cc00@5f455d91

scala> val plus1 = HOModule.partial(1, plus)
val plus1: Int => Int = HOModule$$$Lambda$1710/0x000000080115d7c8@1368ed98

scala> plus1(5)
val res11: Int = 6
*/



//Curying and uncurying 
def mul(x: Int, y: Int) = x * y 
def cmul(x: Int)(y: Int) = x * y



/*
Algebraic Data Types 
*/
object List:
    enum List[+A]: //+A veut dire que A est un covariant de List
        case Nil
        case Cons(head: A, tail: List[A])

    def apply[A](as: A*): List[A] =
        if(as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def product(doubles: List[Double]): Double =
        doubles match
            case Nil => 1
            case Cons(0.0, _) => 0.0
            case Cons(first, rest) => first* product(rest)


    def append[A](l1: List[A], l2: List[A]): List[A] = l1 match
        case Nil => l2
        case Cons(h, t) => Cons(h, append(t, l2))

    def allButLast[A](l: List[A]): List[A] = l match
        case Nil => Nil // We can argue over this :-)
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, allButLast(t))

    def sum(integers: List[Int]):Int = integers match
        case Nil =>0
        case Cons(first, rest)=> first + sum(rest)

    def foldRight[A,B](as: List[A], z: B)(f: (A,B)=>B):B = as match
        case Nil => z
        case Cons(h,t)=> f(h, foldRight(t,z)(f))

    def product2(doubles: List[Double]) =
        foldRight(doubles, 1.0)(_*_)

    def sum2(integers: List[Int]) =
        foldRight(integers, 0)(_+_)
    
end List

object BinaryTree:
    enum Tree[+A]
        case Leaf(value: A)
        case Branch(left: Tree[A], right: Tree[A])
        def size: Int = this match
            case Leaf(_) => 1
            case Branch(l, r) => 1 + l.size + r.size