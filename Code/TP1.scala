//https://www.swisstransfer.com/d/db3cb3fc-802f-4f9d-b6d4-fd81c8c26cb7
//EX1
def square(x: Int): Int = {
  x * x
}
def cube(x: Int): Int = {
  x * x * x
}


//EX2
object TP1:
  def square(x: Int): Int =
    x * x
  def cube(x: Int): Int =
    x * x * x
end TP1 // This is the end marker of TP1


//EX3
def roots(a: Double, b: Double, c: Double): List[Double] = {
  val t = math.pow(b, 2) - (4 * a * c)
  if (t < 0) List[Double]()
  else if (t == 0) List(-b / (2 * a))
  else List((-b - math.sqrt(t)) / (2 * a), (-b + math.sqrt(t)) / (2 * a))
}


//EX4
def sumInt1(n: Int): Int =
  if n == 0 then 0
  else n + sumInt1(n - 1)
  
def sumInt2(n: Int): Int =
  @annotation.tailrec
    def iter(n: Int, acc: Int): Int =
    if n == 0 then acc
    else iter(n - 1, acc + n)
  iter(n, 0)

