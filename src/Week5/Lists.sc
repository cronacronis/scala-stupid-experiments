package Week5

object Lists {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x < y) x :: xs else y :: insert(x, ys)
  }                                               //> insert: (x: Int, xs: List[Int])List[Int]

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }                                               //> isort: (xs: List[Int])List[Int]
  
	/*def remoteAt[T](xs: List[T], n: Int): List[T] = {
		if (n < 0 | n > xs.length - 1) xs //throw new IllegalArgumentException("n is too big for given list")
		xs match {
			case List() => List()
			case List(x) => List()
			case y :: ys => if(n == 0) ys else y :: remoteAt(ys, n - 1)
		}
	} */
	
	def remoteAt[T](xs: List[T], n: Int): List[T] = (xs take n) ::: (xs drop n + 1)
                                                  //> remoteAt: [T](xs: List[T], n: Int)List[T]
	
	
  def init[T](xs: List[T]): List[T] = xs match {
  	case List() => throw new NoSuchElementException("No elements")
  	case List(x) => List()
  	//case y :: ys => if (ys.length == 1) y :: Nil else y :: init(ys)
  	case y :: ys => y :: init(ys)
  }                                               //> init: [T](xs: List[T])List[T]
  
  val testList = List( 1, 2 ,3,4,5)               //> testList  : List[Int] = List(1, 2, 3, 4, 5)
  4 :: testList                                   //> res0: List[Int] = List(4, 1, 2, 3, 4, 5)
	insert(2, testList)                       //> res1: List[Int] = List(1, 2, 2, 3, 4, 5)
	init(testList)                            //> res2: List[Int] = List(1, 2, 3, 4)

	remoteAt(testList, 1)                     //> res3: List[Int] = List(1, 3, 4, 5)
	remoteAt(testList, 4)                     //> res4: List[Int] = List(1, 2, 3, 4)

testList drop 4                                   //> res5: List[Int] = List(5)
testList take 3                                   //> res6: List[Int] = List(1, 2, 3)
}