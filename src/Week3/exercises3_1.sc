package week3

object exercises3_1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  Empty == Empty                                  //> res0: Boolean = true

	val t1 = new NonEmpty(1, Empty, Empty)    //> t1  : week3.NonEmpty = {.1.}
	val t2 = new NonEmpty(2, Empty, Empty)    //> t2  : week3.NonEmpty = {.2.}
	val t3 = new NonEmpty(3, Empty, Empty)    //> t3  : week3.NonEmpty = {.3.}
	
	val t12 = t1 union t2                     //> t12  : week3.IntSet = {{.1.}2.}
	val t21 = t2 union t1                     //> t21  : week3.IntSet = {.1{.2.}}
	
	t1 contains 3                             //> res1: Boolean = false
	
	
	t1.elem                                   //> res2: Int = 1
	t1.left                                   //> res3: week3.IntSet = .
	t2.right                                  //> res4: week3.IntSet = .
	
	t1 union t1                               //> res5: week3.IntSet = {.1.}
	t1.left union t2.right                    //> res6: week3.IntSet = .
	
  //val t123 = t1 union t2 union t3
	 // union t3
 	//t12.left.union(t12.right)
  
}

abstract class IntSet {

	def incl(x: Int): IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
	
}

object Empty extends IntSet {

	def contains(x: Int) = false
	def incl(x: Int) = new NonEmpty(x, Empty, Empty)
	override def toString = "."
	def union(other: IntSet): IntSet = other
	
}

class NonEmpty(val elem: Int, val left: IntSet, val right: IntSet) extends IntSet {

	def contains(x: Int): Boolean = {
		if (x < elem) left contains x
		else if (x > elem) right contains x
		else true
	}
	
	def incl(x: Int): IntSet = {
		if (x < elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this
	}
	override def toString = "{" + left + elem + right + "}"
	
	def union(other: IntSet): IntSet = ((left union right) union other) incl elem
}