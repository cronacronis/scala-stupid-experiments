package Week6
import scala.math.abs
import scala.math.{ max, min }
object nqueens {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val coordinates = (row-1 to 0 by -1) zip queens
    coordinates forall {
      case (oRow, oCol) =>
        val (hor, ver) = (abs(row - oRow), abs(col - oCol))
        if (max(hor, ver) >= 2 & min(hor, ver) >= 1) true
        else false
    }
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }
    placeQueens(n)
  }                                               //> queens: (n: Int)Set[List[Int]]
  def show( queens: List[Int]) = {
  	val lines =
  		for (col <- queens.reverse)
  		yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  		"\n" + (lines mkString "\n")
  }                                               //> show: (queens: List[Int])String
  val solutions = queens(8)                       //> solutions  : Set[List[Int]] = Set(List(1, 4, 6, 2, 0, 5, 3, 7), List(6, 2, 0
                                                  //| , 4, 1, 5, 7, 3), List(3, 5, 0, 4, 2, 6, 1, 7), List(1, 5, 3, 6, 4, 0, 2, 7)
                                                  //| , List(4, 1, 5, 2, 6, 3, 7, 0), List(1, 4, 2, 5, 7, 3, 0, 6), List(7, 4, 1, 
                                                  //| 5, 3, 0, 6, 2), List(1, 4, 0, 5, 7, 3, 6, 2), List(2, 6, 3, 1, 5, 0, 4, 7), 
                                                  //| List(5, 3, 7, 4, 1, 6, 0, 2), List(2, 6, 0, 5, 7, 3, 1, 4), List(5, 0, 4, 6,
                                                  //|  3, 1, 7, 2), List(2, 4, 1, 3, 6, 0, 7, 5), List(0, 2, 4, 6, 3, 1, 7, 5), Li
                                                  //| st(0, 4, 6, 3, 1, 5, 7, 2), List(2, 7, 5, 3, 0, 6, 4, 1), List(5, 1, 3, 7, 4
                                                  //| , 6, 2, 0), List(2, 4, 6, 0, 3, 7, 5, 1), List(5, 0, 7, 2, 4, 1, 6, 3), List
                                                  //| (1, 4, 6, 2, 7, 5, 0, 3), List(0, 5, 2, 7, 4, 1, 3, 6), List(3, 0, 6, 4, 7, 
                                                  //| 1, 5, 2), List(2, 5, 3, 7, 1, 6, 4, 0), List(2, 5, 7, 3, 0, 6, 1, 4), List(7
                                                  //| , 5, 0, 3, 6, 2, 4, 1), List(1, 5, 7, 3, 0, 6, 4, 2), List(2, 0, 6, 4, 7, 1,
                                                  //|  3, 5), List(6, 2, 0, 3, 1, 5, 7, 4), List(6, 0, 4, 2, 7, 1, 3, 5), List(6, 
                                                  //| 1, 4, 2, 7, 3, 0, 5), Li
                                                  //| Output exceeds cutoff limit.
  solutions take 3 foreach { sol => println(show(sol))}
                                                  //> 
                                                  //| * * * * * * * X 
                                                  //| * * * X * * * * 
                                                  //| * * * * * X * * 
                                                  //| X * * * * * * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * * * X * 
                                                  //| * * * * X * * * 
                                                  //| * X * * * * * * 
                                                  //| 
                                                  //| * * * X * * * * 
                                                  //| * * * * * * * X 
                                                  //| * * * * * X * * 
                                                  //| * X * * * * * * 
                                                  //| * * * * X * * * 
                                                  //| X * * * * * * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * * * X * 
                                                  //| 
                                                  //| * * * * * * * X 
                                                  //| * X * * * * * * 
                                                  //| * * * * * * X * 
                                                  //| * * X * * * * * 
                                                  //| * * * * X * * * 
                                                  //| X * * * * * * * 
                                                  //| * * * * * X * * 
                                                  //| * * * X * * * * 
  //val solutions = List(3, 1)

  //isSafe(0, solutions)
  //isSafe(2, 0 :: solutions)
}