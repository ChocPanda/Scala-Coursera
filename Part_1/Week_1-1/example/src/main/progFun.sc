import scala.annotation.tailrec

def abs(x: Double) = if (x < 0) -x else x
  def abs(x: BigDecimal) = if (x < 0) -x else x

  def sqrt(x: Double): Double = {
    def isGoodEnough(guess: Double): Boolean = {
      abs(guess * guess - x) / x < 0.0001
    }

    def improve(guess: Double) =
      (guess + x / guess) / 2

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) { guess }
      else { sqrtIter(improve(guess)) }

    sqrtIter(1.0)
  }

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  gcd(14, 21)

  def factorial(n: Int): Int = {
    @tailrec
    def tailFact(acc: Int, n: Int): Int = {
      if (n == 0) acc else tailFact(acc * n, n - 1)
    }

    tailFact(1, n)
  }

  factorial(4)
