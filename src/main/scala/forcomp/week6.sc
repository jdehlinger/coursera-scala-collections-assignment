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






