object TP1 {
	// EX 3 Find the roots of a quadratic equation
	def roots(a: Int, b: Int, c: Int): List[Double] = {
		if ( b*b - 4*a*c == 0) return List[-1];
		else List[(-b + math.sqrt(b*b - 4*a*c) )/2*a ,(-b - math.sqrt(b*b - 4*a*c) )/2*a ]
	}


}