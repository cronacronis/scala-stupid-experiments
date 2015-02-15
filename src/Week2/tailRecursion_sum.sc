package week2

object tailRecursion_sum {

	def sum(f: Int => Int, a: Int, b:Int) = {
		def loop(i: Int, acc: Int):Int =
			if(i > b) acc
			else loop(i + 1, acc + f(i))
		loop(a, 0)
	}                                         //> sum: (f: Int => Int, a: Int, b: Int)Int
	
	def sumInts(a: Int, b: Int) = sum(x => x, a, b)
                                                  //> sumInts: (a: Int, b: Int)Int
	sumInts(1, 10)                            //> res0: Int = 55
	
	sum(x=> x * x, 1, 10)                     //> res1: Int = 385
}