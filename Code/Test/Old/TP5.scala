enum Tree[+A]:
	case Leaf(value: A)
	case Branch(left: Tree[A], right: Tree[A])

object Tree: 
	def size[A](t: Tree[A]): Int = t match{
		case Leaf(_) => 1
		case Branch(l , r) => 1 + size(l) + size(r)
	}
	/*
	! Lorsqu'on utilise l'extension, on ne répète pas l'input dans la signature de la fonction
	! Cela crée automatiquement des méthodes qu'on appelle avec obj.meth
	*/
	extension (t: Tree[Int])

		/*
		$ Cette fpnction est totale, car il y a au moins un int dans l'arbre
		*/
		def biggestInt: Int = t match{
			case Leaf(l) => l
			case Branch(l, r) => 
				if (l.biggestInt>= r.biggestInt) l.biggestInt
				else r.biggestInt
		}

		// $ Cette fonction n'est pas totale, car il n'y a pas forcément du positif dans l'arbre
		def firstPositive: Option[Int] = t match {
			case Leaf(i) => if i > 0 then Some(i) else None
			case Branch(l, r) => l.firstPositive orElse r.firstPositive
		}

	extension (t: Tree[String])
		// $ Cette focntion est totale car l'arbre contien au moins une string 
		def esrever: Tree[String] = t match {
			case Leaf(s) => Leaf(s.reverse)
			case Branch(l, r) => Branch(l.esrever, r.esrever)
		}

import Tree._
val ti = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
val ts = Branch(Branch(Leaf("01"), Leaf("02")), Branch(Leaf("03"), Leaf("04")))

