//EX1 

enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])

object List:
    def foldRight[A,B](l: List[A], z: B, f: (A,B)=>B): B = 1 match
        case Nil => z
        cose Cons(h, t) => f(h, foldRight(t, z, f))

    def append[A](l1: List[A], l2: List[A]): List[A] = l1 match
        case Nil => l2
        case Cons(h, t) => Cons(h, append(t, l2))
    
    def apply[A](l: A*): List[A] =
        if(l.isEmpty) Nil
        else Cons(l.head, apply(l.tail: _*))
        
    def concat[A](l: List[List[A]]): List[A] = ???
    def map[A,B](l: List[A], f: A => B): List[B] = ???
    def fmap[A,B](l: List[A], f: A => List[B]): List[B] = ???
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???
        