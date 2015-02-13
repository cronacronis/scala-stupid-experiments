package Week6

object Maps {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

	val p1 = new Poly(1 -> 2, 3 -> 4, 5 -> 6.2)
                                                  //> p1  : Week6.Poly = 6.2x^5 + 4.0x^3 + 2.0x^1
	val p2 = new Poly(0 -> 3, 3-> 7)          //> p2  : Week6.Poly = 7.0x^3 + 3.0
	p1 + p2                                   //> res0: Week6.Poly = 6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0
	//p1 +++ p2
}


class Poly(terms0: Map[Int, Double]) {
	
	def this(bindings: (Int, Double)*) = this(bindings.toMap)
	
	val terms = terms0.withDefaultValue(0.0)
	
	//def +++ (other: Poly) = new Poly(terms ++ (other.terms map adjust))
	
	def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
	
	def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
			val (exp, coeff) = term
			terms + (exp -> (coeff + terms.getOrElse(exp, 0.0)))
	}

	
	def adjust(term: (Int, Double)): (Int, Double) = {
		val (exp, coeff) = term
		exp -> (coeff + terms(exp))
	}
	
	
	def makeLabel(exp:Int, coeff: Double): String = exp match {
		case 0.0 => coeff.toString
		case _ => coeff + "x^" + exp
	}
	
	override def toString = {
		( for{
				(exp, coeff) <- terms.toList.sorted.reverse}
			yield makeLabel(exp, coeff) ) mkString " + "
	}
	
	
}