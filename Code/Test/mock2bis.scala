def g2[A, B](l: List[A], t: A => Boolean, f: A => B): List[B] =
	if l.isEmpty then Nil
	else if t(l.head) then f(l.head) :: g2(l.tail, t, f)
	else g2(l.tail, t, f)