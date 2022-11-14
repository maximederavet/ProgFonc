object StructuralRecursion:

    def times(n: Int, m: Int): Int =
        if(n == 0) 0
        else m + times(n - 1, m)

    //Let's rewrite it with the structural pattern
    def times(n: Int, m: Int): Int =
        def F(n: Int, u1: Int): Int =
            if(n == 0) G(u1)
            else H(F(n - 1, K1(n, u1)), n, u1)

        def G(u1: Int) = 0

        def K1(n: Int, u1: Int) = u1
        
        def H(vrec: Int, n: Int, u1: Int) =
            u1 + vrec
        
        F(n, m)

    def fac(n: Int) =
        def F(n: Int): Int =
            if(n == 0) G()
            else H(F(n-1), n)
        def G() = 1
        def H(vrec: int, n: Int)=
            n*vrec
        F(n)

    //flat recursion
    def countif[A](l: List[A], f: A => Boolean): Int =
        if(l.isEmpty) 0
        else (if f(l.head) then 1 else 0) + countif(l.tail, f)

    def countifPattern[A](l: List[A], f: A => Boolean): Int =
        def F[A](l: List[A], u1: A => Boolean): Int =
            if(l.isEmpty) G(u1)
            else H(F(l.tail, K1(l, u1)), l, u1)

        def G[A](u1: A => Boolean) = 0
        def K1[A](l: List[A], u1: A => Boolean) = u1
        def H[A](vrec: Int, l: List[A], u1: A => Boolean) =
            (if u1(l.head) then 1 else 0) + vrec

        F(l, f)

    //QUand il n'y a pas d'arguments additionels le template devient simple
    def count[A](l : List[A]): Int = 
        def F[A](l: List[A]): Int =
            if(l.isEmpty) G()
            else H(F(l.tail), 1)
        
        def G[A]()= 0
        def H[A](vrec: Int, l:List[A])=
            vrec + 1

        F(l)

    def length[A](l: List[A]): Int =
        if(l.isEmpty) 0
        else 1 + length(l.tail)

    def append[A](l1: List[A], l2: List[A]): List[A] =
        if(l1.isEmpty) l2
        else l1.head :: append(l1.tail, l2)

    def reverse[A](l: List[A]): List[A] =
        if(l.isEmpty) Nil
        else append(reverse(l.tail), List(l.head))

    def countifrev[A](l: List[A], f: A => Boolean): Int =
        def F[A](l: List[A], u1: A => Boolean): Int = l match
            case Nil => G(u1)
            case _ => H(F(l.tail, K1(l, u1)), l, u1)

        def G[A](u1: A => Boolean) = 0
        def K1[A](l: List[A], u1: A => Boolean) = u1
        def H[A](vrec: Int, l: List[A], u1: A => Boolean) =
            (if u1(l.head) then 1 else 0) + vrec

        F(l, f)

    def deepRev(l: List[_]): List[_] = l match
        case Nil => Nil
        case (h: List[_]) :: t => append(deepRev(t), List(deepRev(h)))
        case (h: Any) :: t => append(deepRev(t), List(h))

end StructuralRecursion

object VariadicFunctions:

    //Int* indique que la fonction peut prendre autant d'argument qu'on veut
    def sum(args: Int*): Int = args.fold(0)(_+_)

    /*
    scala> sum(3, 6, 9)
    val res0: Int = 18

    scala> sum()
    val res1: Int = 0
    */

end VariadicFunctions

object CompleteStructuralRecursion:

    def exp(m: Int, n: Int): Int =
        if n == 0 then 1
        else if n % 2 == 0 then exp(m * m, n / 2)
        else m * exp(m, n - 1)

    def fib(n: Int): Int =
        if n < 2 then n
        else fib(n - 1) + fib(n - 2)

    def fib2(n: Int): Int =
        @annotation.tailrec
        def helper(n: Int, a: Int, b: Int): Int =
            if(n == 0) then a
            else helper(n - 1, b, a + b)
            helper(n, 0, 1)

    def gcd(a: Int, b: Int): Int =
        if a == 0 then b
        else if b == 0 then a
        else gcd(b, a % b)

    def gcd2(a: Int, b: Int): Int =
        def helper(n: Int, a: Int, b: Int): Int =
            if a % n == 0 && b % n == 0 then n
            else helper(n - 1, a, b)
        helper(a, a, b)

    def zip[A, B](l1: List[A], l2: List[B]): List[(A,B)] = (l1,l2) match
        case (Nil, _) => Nil
        case (_, Nil) => Nil //cas de bases
        case (h1 :: t1, h2 :: t2) => (h1, h2) :: zip(t1, t2)

    /*
        scala> zip(List(1,2,3,4),List("Victor","Bettina","Gaston"))
        val res0: List[(Int, String)] = List((1,Victor), (2,Bettina), (3,Gaston))
    */