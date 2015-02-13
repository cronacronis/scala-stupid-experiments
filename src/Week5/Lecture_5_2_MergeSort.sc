package Week5
import scala.math.Ordering

object Lecture_5_2_MergeSort {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def merge[T](xs: List[T], ys: List[T])(implicit ord: Ordering[T]): List[T] = {
    (xs, ys) match {
      case (Nil, Nil) => List()
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (x :: xst, y :: yst) =>
        if (ord.lt(x, y)) x :: merge(xst, ys)
        else y :: merge(xs, yst)
    }
  }                                               //> merge: [T](xs: List[T], ys: List[T])(implicit ord: scala.math.Ordering[T])Li
                                                  //| st[T]

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }                                               //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

  val someList = List(1, 1, 6, -4, 6, 7, -4, 0)   //> someList  : List[Int] = List(1, 1, 6, -4, 6, 7, -4, 0)
  someList span (x => x > 0)                      //> res0: (List[Int], List[Int]) = (List(1, 1, 6),List(-4, 6, 7, -4, 0))

  val toPack = List("a", "a", "a", "b", "c", "c", "a")
                                                  //> toPack  : List[String] = List(a, a, a, b, c, c, a)
	someList.span(x => x > 0)._1              //> res1: List[Int] = List(1, 1, 6)
 
  def pack[T](xs: List[T])(implicit ord: Ordering[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xt => {
    	val (matched, notMatched) = xs.span(xf => ord.equiv(x, xf))
    		matched :: pack(notMatched)
  	}
  }                                               //> pack: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[List[T]]
	pack(toPack)                              //> res2: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a)
                                                  //| )
	
	
	def encode[T](xs: List[T])(implicit ord: Ordering[T]): List[(T, Int)] = {
		pack(xs).map( valList => (valList.head, valList.length) )
	}                                         //> encode: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[(T, Int)
                                                  //| ]

	encode(toPack)                            //> res3: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
	encode(List[Int]())                       //> res4: List[(Int, Int)] = List()

	def mapFun[T, U](xs: List[T], f: T => U): List[U] =
		(xs foldRight List[U]()) ({case(value, newList) => f(value)::newList})
                                                  //> mapFun: [T, U](xs: List[T], f: T => U)List[U]
		
	def lengthFun[T](xs: List[T]): Int = (xs foldRight 0) ({case(value, counter) => counter + 1})
                                                  //> lengthFun: [T](xs: List[T])Int

	lengthFun(toPack)                         //> res5: Int = 7
	
	
	mapFun[String, String](toPack, (x => x.length.toString))
                                                  //> res6: List[String] = List(1, 1, 1, 1, 1, 1, 1)

}
    
      