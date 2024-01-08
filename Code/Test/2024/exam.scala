def compute[A,B](l: List[A], f: (A,A)=>B): List[B] = l match
	case Nil => Nil
	case h::Nil => Nil
	case h::t => f(h, t.head)::compute(t,f)


def compute2[A,B](list: List[A], f: (A,A)=>B): List[B] = 

	@annotation.tailrec
	def iter(l: List[A], acc: List[B]): List[B] = l match //! Attention pas besoin de réécrire les types [A,B]
		case head::next::tail => iter(next::tail, acc:+f(head, next))
		case _ => acc
		
	
	iter(list, List[B]())

/* Cette récursivité est structurelle complète, car son cas de base est différent de Nil etc.. */


def tabulate[A](rows: Int, columns: Int, f: (Int, Int) => A): List[List[A]] = 
	def iter1(i: Int): List[List[A]] = 
		def iter2(j: Int): List[A] = 
			if(j == columns+1) Nil else f(i,j)::iter2(j+1)

		if (i == rows + 1) Nil else iter2(1) :: iter1(i+1)
	
	iter1(1)

// Cette fonction est récusrive structurelle complète, car il y a deux appels récusrifs, et les cas de bases ne sont pas 0 


/* ADTs */
case class Observation(date: String, minimum: Double, maximum: Double)

def avg(obs: Observation): Double = 
	(obs.minimum + obs.maximum)/2

def foo(observations: List[Observation]): List[Double] = observations.filter(_.minimum>13).map( (o: Observation) => avg(o))


enum Shapes[Int]: 
	case Circle(r: Int)
	case Rectangle(l: Int, lar: Int)
	case Square(c: Int)

	def area:Double = this match
		case Circle(r) => r * r * 3.14
		case Rectangle(l,la) => l*la
		case Square(c) => c*c

	
def bar(shapes: List[Shapes]): Double = 
	shapes.foldRight((0.0,0))((s:Shapes, acc: Double) => if (s.area > 4.0) (acc._1 + s.area, acc._2 + 1) else acc) match {
		case (sum, count) => sum/count
		case _ => 0
	}


import Either._

def inverse(i: Int): Either[String, Double] =
	if (i != 0) Right(1.0 / i)
	else Left("Cannot take the inverse of 0.")

def process(s: String): Either[String, Double] =
	parse(s).flatMap(inverse)