package KnightsTour

/* Another famous problem is this one: How can a knight jump on an N×N chessboard in such a way that it visits every
  square exactly once?

  Hints: Represent the squares by pairs of their coordinates of the form (X, Y), where both X and Y are
  integers between 1 and N. (Alternately, define a Point class for the same purpose.) Write a function jumps(N, (X, Y))
  to list the squares that a knight can jump to from (X, Y) on a N×N chessboard. And finally, represent the solution of
  our problem as a list of knight positions (the knight's tour).

  It might be nice to find more than one tour, but a computer will take a long time trying to find them all at once.
  Can you make a lazy list that only calculates the tours as needed?

  Can you find only "closed tours", where the knight can jump from its final position back to its starting position? */

case class Point(x: Int, y: Int){
  def +(o : Point) = Point(x + o.x, y + o.y)
  def -(o : Point) = Point(x - o.x, y - o.y)
  def +(t: (Int,Int)) = Point(x + t._1, y + t._2)
  def -(t: (Int,Int)) = Point(x - t._1, y - t._2)
  def jumps(n : Int): List[Point] = List((2,1),(1,2),(-1,2),(-2,1)) flatMap {x => List(this + x, this - x)} filter {p => (p.x min p.y) >= 1 && (p.x max p.y) <= n}
}


