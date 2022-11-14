//Le code de la factorielle du chap1 n'est pas une récursion terminale. Transformons-le
object FactorialModule:
    def fac(n: Int): Int =

        @annotation.tailrec  //Si une fonction est de récursivité terminale, le développeur toi le préciser pour que le compilateur puisse optimiser le code
        def iter(n: Int, acc: Int): Int =
            if(n == 0) acc
            else iter(n-1, n*acc)
    
    iter(n, 1)

    private def formatFac(n: Int) = { //Private n'est pas important pour ce cours-ci 
        val messsage = "The ffactorial of %d is %d"
        message.format(n, fac(n))
    }

    def main(args: Array[String]): Unit =
        println(formatFac(5))


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