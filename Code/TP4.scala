//EX1
def fib1(n: Int): Int = {
if (n <= 1) n
else fib1(n - 1) + fib1(n - 2)
}

def fib2(n: Int): Int = {
@annotation.tailrec
def loop(n: Int, a: Int, b: Int): Int = {
if (n == 0) then a
else loop(n - 1, b, a + b)
}
loop(n, 0, 1)
}


//EX2
// for a natural number n, sumInt1(n) = n + n-1 + ... + 0
def sumInt1(n: Int): Int =
if n == 0 then 0
else n + sumInt1(n - 1)
// sumInt1 is not tail recursive as the result of the recursive call
// is used in an expression. In other words, the result of the recursive
// call is not returned immediately.
// for a natural number n, sumIn21(n) = n + n-1 + ... + 0
def sumInt2(n: Int): Int =
// for a natural number n and an integer acc, iter(n, acc) = n + n-1 + ... + acc
@annotation.tailrec
def iter(n: Int, acc: Int): Int =
if n == 0 then acc
else iter(n - 1, acc + n)
iter(n, 0)



//EX3
// For an integer x and an natural number n, power1(x,n) = x^n
def power1(x: Int, n: Int): Int =
if n == 0 then 1
// else if n % 2 == 0 then power1(x * x, n / 2)
else x * power1(x, n - 1)
// power1 is not tail recursive as the result of the recursive call
// is used in an expression. In other words, the result of the
// recursive call is not returned immediately.
// For an integer x and an natural number n, power1(x,n) = x^n
def power2(x: Int, n: Int): Int =
// For an integer x, a natural number n, and an integer acc,
// iter(x, n, acc) = x^n * acc
@annotation.tailrec
def iter(x: Int, n: Int, acc: Int): Int =
if n == 0 then acc
// else if n % 2 == 0 then iter(x * x, n / 2, acc)
else iter(x, n - 1, acc * x)
iter(x, n, 1)
// The addition of the extra recursive step on indirect components '
// of n turns this function that relies on structural recursion
// into a function that relies on complete structural recursion.