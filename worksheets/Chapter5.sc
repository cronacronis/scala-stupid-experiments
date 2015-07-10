import fpinscala.Chapter5._
object Chapter5 {
	import SomeFunctions._
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	maybeTwice(true, { println("hi"); 1+41 }) //> hi
                                                  //| hi
                                                  //| res0: Int = 84
	maybeTwice2(true, { println("hi"); 1+41 })//> hi
                                                  //| res1: Int = 84

	val str = Stream(1,2,3,4)                 //> str  : fpinscala.Chapter5.Stream[Int] = Cons(<function0>,<function0>)
	str.toList                                //> res2: List[Int] = List(1, 2, 3, 4)
	//val str2 = Stream.cons(1, Empty)
	//str2 cut 2
	str cut 1                                 //> hit main cycle
                                                  //| hit n
                                                  //| res3: (List[Int], fpinscala.Chapter5.Stream[Int]) = (List(1),Cons(<function0
                                                  //| >,<function0>))
	str take 3                                //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit n
                                                  //| res4: List[Int] = List(1, 2, 3)
	str drop 2                                //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit n
                                                  //| res5: fpinscala.Chapter5.Stream[Int] = Cons(<function0>,<function0>)
}