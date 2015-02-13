package Week6

object Pairs {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	def isPrime(n: Int): Boolean = {
		(2 until n) foreach ( d => if(n.toDouble % d.toDouble == 0) return false )
		true
	}                                         //> isPrime: (n: Int)Boolean
  val n = 7                                       //> n  : Int = 7

	(1 until n) flatMap (i =>
		(1 until i) map (j => (i, j))) filter ( pair =>
			isPrime(pair._1 + pair._2))
                                                  //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))

	for {
		i <- 1 until n
		j <- 1 until i
		if isPrime(i + j)
	} yield (i + j)                           //> res1: scala.collection.immutable.IndexedSeq[Int] = Vector(3, 5, 5, 7, 7, 7, 
                                                  //| 11)
	def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
		(for ((x, y) <- xs zip ys)	yield x * y).sum
	}                                         //> scalarProduct: (xs: List[Double], ys: List[Double])Double
	scalarProduct(List(1,2), List(3,4))       //> res2: Double = 11.0
}