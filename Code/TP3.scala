//EX1 

enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])

object List:

    def foldRight[A,B](l: List[A], z: B, f: (A,B)=>B): B = l match
        case Nil => z
        case Cons(h, t) => f(h, foldRight(t, z, f))

    def append[A](l1: List[A], l2: List[A]): List[A] = l1 match
        case Nil => l2
        case Cons(h, t) => Cons(h, append(t, l2))
    
    def apply[A](l: A*): List[A] = 
        if(l.isEmpty) Nil
        else Cons(l.head, apply(l.tail: _*))
        

    //Concat sould take as imput a list of lists, and return a whole complete list    
    def concat[A](l: List[List[A]]): List[A] = l match
        case Nil => Nil
        case Cons(h,t) => append(h, concat(t))



    def map[A,B](l: List[A], f: A => B): List[B] = l match
        case Nil => Nil
        case Cons(h,t) => Cons(f(h), map(t,f))


    def fmap[A,B](l: List[A], f: A => List[B]): List[B] = l match
        case Nil => Nil
        case Cons(h,t) => append(f(h), fmap(t,f))

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _=> l


//EX 2
/*
scala> map(x, _+3)
val res7: List[Int] = Cons(4,Cons(5,Cons(6,Cons(7,Nil))))
                                                                                                                                                         
scala> fmap(x, x=>List(x+3.5))
val res8: List[Double] = Cons(4.5,Cons(5.5,Cons(6.5,Cons(7.5,Nil))))
                                                                                                                                                         
scala> dropWhile(x, _<=3)
val res9: List[Int] = Cons(4,Nil)
                                                                                                                                                         
scala> concat(List(x,x))
val res10: List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(1,Cons(2,Cons(3,Cons(4,Nil))))))))
*/

//EX3 Rewrite all the upper function using foldRight

    def concat2[A](l: List[List[A]]): List[A] =
        foldRight(l, Nil: List[A], append)

    def map2[A,B](l: List[A], f: A => B): List[B] =
        foldRight(l, Nil: List[B], (a, acc) => Cons(f(a), acc))

    def fmap2[A,B](l: List[A], f: A => List[B]): List[B] =
        foldRight(l, Nil: List[B], (a, acc) => append(f(a), acc))
        
    def append2[A](l1: List[A], l2: List[A]): List[A] =
        foldRight(l1, l2, (l, acc) => Cons(l, acc))

enum Tree[+A]:
    case Leaf(value: A)
    case Branch(left: Tree[A], right: Tree[A])
/*
object tree: 
    import Tree._
    import List._
    
    def size: Int = this match 
        case Leaf(_) => 1
        case Branch(l, r) => 1 + l.size + r.size

    def depth: Int = this match
        case Leaf(_) => 0
        case Branch(l, r) => 1 + (l.depth.max(r.depth))

    def map[A,B](f: A => B): Tree[B] = this match
        case Leaf(a) => Leaf(f(a))
        case Branch(l, r) => Branch(l.map(f), r.map(f))
    
    def fold[A,B](f: A => B, g: (B,B) => B): B = this match
        case Leaf(a) => f(a)
        case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

    def size2: Int =
        fold(a => 1, 1 + _ + _)

    def depth2: Int =
        fold(a => 0, (d1, d2) => 1 + (d1.max(d2)))
        
    def map2[A,B](f: A => B): Tree[B] =
        fold(a => Leaf(f(a)), Branch(_, _))

        */