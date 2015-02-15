package week2

object factorial2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def factorial(x: Int) =
	{
		def loop(acc: Int, n: Int):Int =
			if(n == 0) acc
			else loop(acc * n, n - 1)
		loop(1, x)
	}                                         //> factorial: (x: Int)Int
	factorial(5)                              //> res0: Int = 120
	
	
}