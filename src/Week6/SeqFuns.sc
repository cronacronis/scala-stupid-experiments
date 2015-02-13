package Week6

object SeqFuns {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	
	def isPrime(n: Int): Boolean = {
		(2 until n) foreach ( d => if(n.toDouble % d.toDouble == 0) return false )
		true
	}                                         //> isPrime: (n: Int)Boolean
	
	isPrime(3)                                //> res0: Boolean = true
	isPrime(2131)                             //> res1: Boolean = true
	isPrime(4)                                //> res2: Boolean = false

}