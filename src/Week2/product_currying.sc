package week2

object product_currying {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	
	
	def aggr(f: Int => Int, g: (Int, Int) => Int, nullVal: Int)(a: Int, b: Int): Int =
			if (a > b) nullVal
			else g(f(a), aggr(f, g, nullVal)(a + 1, b))
                                                  //> aggr: (f: Int => Int, g: (Int, Int) => Int, nullVal: Int)(a: Int, b: Int)Int
                                                  //| 
	
	
	aggr(x => x, (x,y) => x + y, 0)(1,10)     //> res0: Int = 55
	aggr(x => x, (x,y) => x * y, 1)(1,10)     //> res1: Int = 3628800
	
	def sum(f: Int => Int)(a: Int, b: Int): Int =
			if (a > b) 0
			else f(a) + sum(f)(a + 1, b)
                                                  //> sum: (f: Int => Int)(a: Int, b: Int)Int
	
	
	sum(x => x)(1,10)                         //> res2: Int = 55
	
	

	
	def product(f: Int => Int)(a: Int, b: Int): Int =
			if (a > b) 1
			else f(a) * product(f)(a + 1, b)
                                                  //> product: (f: Int => Int)(a: Int, b: Int)Int
	product(x => x * x)(3, 4)                 //> res3: Int = 144

	def fact(x: Int):Int = {
		product(x => x)(1, x)
	}                                         //> fact: (x: Int)Int
	fact(5)                                   //> res4: Int = 120

}