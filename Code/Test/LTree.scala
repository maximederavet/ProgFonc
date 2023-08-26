enum LTree[+A]:
	case LLeaf(value: A)
	case LBranch(value: A, left: LTree[A], right: LTree[A]) 

	def value: A = this match {
		case LLeaf(l) => l
		case LBranch(l, _, _) => l.value + r.value
	}

enum Tree[+A]:
	case Leaf(value: A)
	case Branch(left: Tree[A], right: Tree[A])

	

object LTree:
	def compute(t: LTree[_]): Double = t match {
		case LBranch( "ADD", l, r) => compute(l) + compute(r)
		case LBranch("SUB", l, r) => compute(l) - compute(r)
		case LBranch("MUL", l, r) => compute(l) * compute(r)
		case LBranch("DIV", l, r) => compute(l) / compute(r)
		case LLeaf(v: Double) =>	v
	}

	def transform(t: Tree[Int], f: (Int, Int) => Int ): LTree[Int] = t match {
		case Leaf(a) => A
		case Branch(left, right) => 
			val l = transform(left, f)
			val r = transform(right, f)
			LBranch(f(l.value, r.value), l , r)
	}



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