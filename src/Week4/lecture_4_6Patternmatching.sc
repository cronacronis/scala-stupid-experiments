package Week4

object lecture_4_6Patternmatching {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

	Sum(Number(1), Number(2)).show            //> res0: String = 1 + 2
	Prod(Number(2), Var("x")).show            //> res1: String = 2 * x
	Sum(Var("x"), Var("y")).show              //> res2: String = x + y
	Prod(Sum(Number(2), Var("x")), Var("x")).show
                                                  //> res3: String = (2 + x) * x
  Prod(Var("x"), Sum(Number(2), Var("x"))).show   //> res4: String = x * (2 + x)

	Prod(Var("x"), Prod(Number(2), Var("x"))).show
                                                  //> res5: String = x * 2 * x

	
  Prod(
  	
  	Var("x"),
  	Sum(
  		Number(2),
  		Prod(
  			Var("x"), Sum(Number(2), Var("x"))))
  ).show                                          //> res6: String = x * (2 + x * (2 + x))
}


trait Expr {
	def eval: Int = this match {
		case Number(n) => n
		case Sum(e1, e2) => e1.eval + e2.eval
		case Prod(e1, e2) => e1.eval * e2.eval
		//case Var(x) => x.asInstanceOf[Int]
	}
	
	def showParenthesis: String = this match {
		case Sum(e1, e2) => "(" + this.show + ")"
		case _ => this.show
	}
	
	def show: String = this match {
		case Number(n) => n.toString
		case Var(x) => x
		case Sum(e1, e2) => e1.show + " + "  + e2.show
		case Prod(e1, e2) => e1.showParenthesis + " * " + e2.showParenthesis
	}
}
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(x: String) extends Expr