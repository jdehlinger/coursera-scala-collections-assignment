import scala.Iterable

def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)

val n = 7

(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (pair =>
  isPrime(pair._1 + pair._2))


def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queensWithRow = (row - 1 until 0 by -1) zip queens
  queensWithRow forall {
    case (r, c) => col != c && math.abs(col - c) != row - r
  }
}


def nqueens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
    for {
      nqueens <- placeQueens(k-1)
      col <- 0 until n
      if isSafe(col, nqueens)
    } yield col :: nqueens
    placeQueens(n)
}

nqueens(4)


class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  def +(other: Poly) = new Poly(other.terms foldLeft terms)(addTerm)

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exponent, coefficient) = term
    terms + (exponent -> (coefficient + terms(exponent)))
  }

  override def toString =
    (for ((exponent, coefficient) <- terms.toList.sorted.reverse) yield coefficient + "x^" + exponent) mkString " + "
}


val poly1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val poly2 = new Poly(0 -> 3.0, 3 -> 7.0))

val result = poly1 + poly2
val result2 = poly1.terms(7)







