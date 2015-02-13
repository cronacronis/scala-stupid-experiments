object Currying {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  def cube(x: Int): Int =
  	x * x * x                                 //> cube: (x: Int)Int
  
  def sum(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) 0 else f(a) + sum(f)(a + 1, b) //> sum: (f: Int => Int)(a: Int, b: Int)Int
  	
  sum(cube)(2, 3)                                 //> res0: Int = 35
  
  def product(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) 1 else f(a) * product(f)(a + 1, b)
                                                  //> product: (f: Int => Int)(a: Int, b: Int)Int
  
  product(cube)(2,3)                              //> res1: Int = 216
  
  
  def factorial(x: Int): Int =
  	product((x: Int) => x)(1, x)              //> factorial: (x: Int)Int
  factorial(5)                                    //> res2: Int = 120
  
	
	
	def mapReduce(f: Int => Int, combine: (Int, Int) => Int, term: Int)(a: Int, b: Int): Int =
		 if (a > b) term else combine(f(a), mapReduce(f, combine, term)(a + 1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, term: Int)(a: Int, b:
                                                  //|  Int)Int
		 
		 
	def combine(g: (Int, Int) => Int, term: Int)(f: Int => Int)(a: Int, b: Int): Int =
		if (a > b) term else g(f(a), combine(g, term)(f)(a + 1, b))
                                                  //> combine: (g: (Int, Int) => Int, term: Int)(f: Int => Int)(a: Int, b: Int)Int
                                                  //| 
		 	//g(f(a), aggr(g, term)(f)(a + 1, b)
	combine( (x:Int, y:Int) => x + y, 0)(cube)(1, 5)
                                                  //> res3: Int = 225
	sum(cube)(1,5)                            //> res4: Int = 225
}