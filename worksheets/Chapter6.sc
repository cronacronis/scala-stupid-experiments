package fpinscala

import fpinscala.Chapter6._
//import scala.Int.MaxValue
object Chapter6_worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	val rng = SimpleRNG(42)                   //> rng  : fpinscala.Chapter6.SimpleRNG = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt                    //> n1  : Int = 16159453
                                                  //| rng2  : fpinscala.Chapter6.RNG = SimpleRNG(1059025964525)
	Int.MaxValue                              //> res0: Int(2147483647) = 2147483647
	Int.MinValue                              //> res1: Int(-2147483648) = -2147483648
	-Int.MinValue                             //> res2: Int = -2147483648
	-(Int.MinValue +1 )                       //> res3: Int = 2147483647

	rng.double01(rng)._1                      //> res4: Double = 0.007524831689672932

  //rng.double3(rng)

  rng.ints(5)(rng)                                //> hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit main cycle
                                                  //| hit n <= 0
                                                  //| res5: (List[Int], fpinscala.Chapter6.RNG) = (List(16159453, -1281479697, -34
                                                  //| 0305902, -2015756020, 1770001318),SimpleRNG(1059025964525))
	rng.double(rng)                           //> res6: (Double, fpinscala.Chapter6.RNG) = (0.007524831689672932,SimpleRNG(105
                                                  //| 9025964525))


	val randList = List.fill(3)(rng.double)   //> randList  : List[fpinscala.Chapter6_worksheet.rng.Rand[Double]] = List(<func
                                                  //| tion1>, <function1>, <function1>)
	val seq = rNG.sequence(randList)(rng)     //> seq  : (List[Double], fpinscala.Chapter6.RNG) = (List(0.007524831689672932, 
                                                  //| 0.596735485175967, 0.15846728401187216),SimpleRNG(259172689157871))
	
	rng.nonNegativeLessThan2(5)(rng)          //> res7: (Int, fpinscala.Chapter6.RNG) = (3,SimpleRNG(1059025964525))

	val zero = rng.rollDie(SimpleRNG(5))._1   //> zero  : Int = 1
	  
}