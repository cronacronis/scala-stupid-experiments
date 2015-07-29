import fpinscala.Chapter5._
object Chapter5 {
	import SomeFunctions._
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	maybeTwice(true, { println("hi"); 1+41 }) //> hi
                                                  //| hi
                                                  //| res0: Int = 84
	maybeTwice2(true, { println("hi"); 1+41 })//> hi
                                                  //| res1: Int = 84
  ///////////////////////
	val str = Stream(1,2,3,4)                 //> str  : fpinscala.Chapter5.Stream[Int] = Cons(<function0>,<function0>)
	///////////////////////
	
	str.toList                                //> res2: List[Int] = List(1, 2, 3, 4)
	val str2 = Stream.cons(1, Empty)          //> str2  : fpinscala.Chapter5.Stream[Int] = Cons(<function0>,<function0>)
	str take(3) toList                        //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| res3: List[Int] = List(1, 2, 3)
	
	
	val takeMore = str take(10) toList        //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| takeMore  : List[Int] = List(1, 2, 3, 4)
	
	
	str drop(2) toList                        //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| res4: List[Int] = List(3, 4)

	str takeWhile(x => x < 3) toList          //> res5: List[Int] = List(3, 4)
 

	val mapped = str map (_ + 1)  toList      //> mapped  : List[Int] = List(2, 3, 4, 5)
	    
	str.filter(_ > 2) toList                  //> res6: List[Int] = List(3, 4)
	
	
	def incr(a: Int) = Stream(a + 10, a + 20) //> incr: (a: Int)fpinscala.Chapter5.Stream[Int]
	
	(str map incr toList) map ( x => x.toList )
                                                  //> res7: List[List[Int]] = List(List(11, 21), List(12, 22), List(13, 23), List(
                                                  //| 14, 24))
  str append str.map(_ + 10) toList               //> res8: List[Int] = List(1, 2, 3, 4, 11, 12, 13, 14)
	
	str.flatMap(incr) toList                  //> res9: List[Int] = List(11, 21, 12, 22, 13, 23, 14, 24)
	
	/*val appended = str.append(5)
	appended toList*/

	val twos = Stream.constant(2)             //> twos  : fpinscala.Chapter5.Stream[Int] = Cons(<function0>,<function0>)

	val b = twos take 5 toList                //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| b  : List[Int] = List(2, 2, 2, 2, 2)
	 val c = Stream.constant2(2) take 5 toList//> hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| c  : List[Int] = List(2, 2, 2, 2, 2)
	
	val fromVals = Stream.from(5).take(10).toList
                                                  //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| fromVals  : List[Int] = List(5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
	val fromVals2 = Stream.from2(5).take(10).toList
                                                  //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| fromVals2  : List[Int] = List(5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
 
	val fibsVals = Stream.fibs take 10 toList //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| fibsVals  : List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
	val fibsVals2 = Stream.fibs2 take 10 toList
                                                  //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| fibsVals2  : List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
	
	print(str toList)                         //> List(1, 2, 3, 4)
	val mapped1 = str map1 (_ + 1)  toList    //> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| hit empty
                                                  //| mapped1  : List[Int] = List(2, 3, 4, 5)
	
	val b1 = twos take1 3 toList              //> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| b1  : List[Int] = List(2, 2, 2)
	val c1 = str takeWhile1(x => x < 3) toList//> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| c1  : List[Int] = List(1, 2)
	val zipped = str zipWith Stream(1,2) toList
                                                  //> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit empty
                                                  //| zipped  : List[(Int, Int)] = List((1,1), (2,2))
	val zippedAll = str zipAll Stream(1,2,3,4,5,6,7,8) toList
                                                  //> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit empty
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit empty
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit empty
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit empty
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| hit empty
                                                  //| hit empty
                                                  //| zippedAll  : List[(fpinscala.Chapter4.Option[Int], fpinscala.Chapter4.Optio
                                                  //| n[Int])] = List((Some(1),Some(1)), (Some(2),Some(2)), (Some(3),Some(3)), (S
                                                  //| ome(4),Some(4)), (None,Some(5)), (None,Some(6)), (None,Some(7)), (None,Some
                                                  //| (8)))

	val hasSub = str hasSubsequence(Stream(1,2,3))
                                                  //> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| hit empty
                                                  //| hasSub  : Boolean = true
	val hasNoSub = str hasSubsequence(Stream(7,8,18,19,40,45))
                                                  //> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| hasNoSub  : Boolean = false

	Stream(1,2,3) startsWith Stream(3,2)      //> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| res10: Boolean = false
	
	Stream(1,2,3).scanRight(0)(_ + _).toList  //> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| res11: List[Int] = List(6, 5)
	
	Stream(1,2,3).scanRight2(0)(_ + _).toList //> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| hit empty
                                                  //| res12: List[Int] = List(6, 5, 3, 0)
	Stream(1,2,3).scanRight3(0)(_ + _).toList //> res13: List[Int] = List(6, 5, 3, 0)
	
	
	def time[R](block: => R) = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    (t1 - t0)
}                                                 //> time: [R](block: => R)Long

	val result3 = time { Stream(1,2,3).scanRight3(0)(_ + _).toList }
                                                  //> Elapsed time: 587900ns
                                                  //| result3  : Long = 587900
	val result2 = time { Stream(1,2,3).scanRight2(0)(_ + _).toList }
                                                  //> hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| hit main cycle
                                                  //| hit empty
                                                  //| hit empty
                                                  //| Elapsed time: 2428200ns
                                                  //| result2  : Long = 2428200
 result2 / result3                                //> res14: Long = 4
}