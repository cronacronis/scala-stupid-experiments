package Week2

object Lecture_2_5 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val x = new Rational(1, 3)                      //> x  : Week2.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : Week2.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : Week2.Rational = 3/2
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  x.neg                                           //> res2: Week2.Rational = -1/3
  x.sub(y)                                        //> res3: Week2.Rational = -8/21
  x.sub(y).sub(z)                                 //> res4: Week2.Rational = -79/42
  y.add(y)                                        //> res5: Week2.Rational = 70/49
}


class Rational(x: Int, y: Int) {
	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
	//private val g = gcd(x, y)
	def numer = x
	def denom = y
	
	def neg = new Rational(-x, y)
	
	def sub(that: Rational) = add(that.neg)
	
	def add(that: Rational) =
		new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom)
			
	override def toString = numer + "/" + denom
}