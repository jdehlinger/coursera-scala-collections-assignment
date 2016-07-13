def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)

val n = 7

isPrime(n+1)

def go(n: Int) =
(1 until n)
  flatMap (i => (1 until 1) map (j => (i, j)))
    filter (pair => isPrime(pair._1 + pair._2))


