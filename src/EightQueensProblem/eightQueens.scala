/*  This is a classical problem in computer science. The objective is to place eight queens on a chessboard
    so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column,
    or on the same diagonal.
    Hint: Represent the positions of the queens as a list of numbers 1..N. Example: List(4, 2, 7, 3, 6, 8, 5, 1)
    means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the
     generate-and-test paradigm.*/

object nQueens{
  def queens(num: Int):Set[List[Int]] = {
    def placeQueens(position: Int):Set[List[Int]] = {
      if (position == 0) Set(List())
      else
        for{
          queens <- placeQueens(position-1)
          col <- 0 until num
          if isSafe(queens,col)
        } yield col :: queens
    }
    placeQueens(num)
  }

  def isSafe(queens: List[Int], col: Int):Boolean ={
    val row = queens.length
    val queensWithRow = (row -1 to 0 by -1) zip queens
    queensWithRow forall{
      case(r,c) => col != c && math.abs(col - c)!= row -r
    }
  }

  def show(queens: List[Int])={
    val lines =
      for (col <- queens.reverse) yield Vector.fill(queens.length)("*").updated(col, "Q").mkString
    "\n" + (lines mkString "\n")
  }

  def main(args: Array[String]): Unit = {
    print((queens(4) map show) mkString "\n")
  }
}