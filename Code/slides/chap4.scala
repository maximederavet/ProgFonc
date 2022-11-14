
def foobar(n: Int): Int =
    // val x: Int = n / (n - 5)
    try
        (n / (n - 5)) + 1
    catch
        case e: Exception => -9999


import Option.{Some, None}

def mean(l: Seq[Double]): Option[Double] =
    if(l.isEmpty) None
    else Some(l.sum / l.length)

/*
scala> mean(List(1,2,3,4,5))
val res0: Option[Double] = Some(3.0) 

scala> mean(List())
val res1: Option[Double] = None
*/


def variance(l: Seq[Double]): Option[Double] = {
    mean(l).flatMap(m => mean(l.map(x => math.pow(x - m, 2))))
}
/*
scala> variance(List())
val res0: Option[Double] = None

scala> variance(List(1,2,3,4,5))
val res1: Option[Double] = Some(2.0)

scala> variance(List(1,2,3,4,5)).getOrElse("Invalid!")
val res2: Matchable = 2.0
*/


enum Option[+A]:
    case Some(get: A)
    case None

def map[B](f: A => B): Option[B] = this match
    case None => None
    case Some(a) => Some(f(a))

def getOrElse[B>:A](default: => B): B = this match
    case None => default
    case Some(a) => a

def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

def orElse[B>:A](op: => Option[B]): Option[B] =
    map(x => Some(x)).getOrElse(op)

def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)



//Slide 17 -> 26 manquantes. Je ne sais pas comment les articuler dans un code proprement
