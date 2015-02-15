package week1

object SquareRoot {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(impove(guess, x), x)            //> sqrtIter: (guess: Double, x: Double)Double

  def isGoodEnough(guess: Double, x: Double) =

    abs(guess * guess - x) < (x * 1e-8)           //> isGoodEnough: (guess: Double, x: Double)Boolean

  def impove(guess: Double, x: Double) =
    (guess + x / guess) / 2                       //> impove: (guess: Double, x: Double)Double

  def sqrt(x: Double) = sqrtIter(1.0, x)          //> sqrt: (x: Double)Double

  sqrt(2)                                         //> res0: Double = 1.4142135623746899

  sqrt(100000)                                    //> res1: Double = 316.2277660203896

  sqrt(1e-5)                                      //> res2: Double = 0.0031622776602038957

  isGoodEnough(sqrt(1e-5), 1e-5)                  //> res3: Boolean = true

  316.2277660203896 * 316.2277660203896           //> res4: Double(100000.00000224626) = 100000.00000224626
  0.0031622776602038957 * 0.0031622776602038957   //> res5: Double(1.0000000000224625E-5) = 1.0000000000224625E-5

  sqrt(0.001)                                     //> res6: Double = 0.03162277660168433
  sqrt(0.1e-10)                                   //> res7: Double = 3.1622776601874535E-6
  sqrt(0.1e-20) * sqrt(0.1e-20)                   //> res8: Double = 1.000000000000003E-21
  sqrt(1.0e20) * sqrt(1.0e20)                     //> res9: Double = 1.0000000000046159E20
  sqrt(1.0e50) * sqrt(1.0e50)                     //> res10: Double = 1.0000000000001448E50

  def factorial(x: Double): Double =
    {
    	if(x == 0) 1 else x * factorial(x - 1)
    }                                             //> factorial: (x: Double)Double


	factorial(5)                              //> res11: Double = 120.0

}