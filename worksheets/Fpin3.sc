import fpinscala.datastructures._
import scala.math._
import List.sum

object Fpin3 {
	println("Welcome to the Scala worksheet") //> Welcome to the Scala worksheet
	val ex1: List[Double] = Nil               //> ex1  : fpinscala.datastructures.List[Double] = Nil
	val ex2: List[Int] = Cons(1, Nil)         //> ex2  : fpinscala.datastructures.List[Int] = Cons(1,Nil)
	val ex3: List[String] = Cons("a", Cons("b", Nil))
                                                  //> ex3  : fpinscala.datastructures.List[String] = Cons(a,Cons(b,Nil))

	List(1, 2, 3) match { case _ ⇒ 42 }       //> res0: Int = 42
	List(1, 2, 3) match { case Cons(h, _) ⇒ h }
                                                  //> res1: Int = 1
	List(1, 2, 3) match { case Cons(_, t) ⇒ t }
                                                  //> res2: fpinscala.datastructures.List[Int] = Cons(2,Cons(3,Nil))
	//List(1,2,3) match { case Nil => 42 }

	val x = List(1, 2, 3, 4, 5) match {
		case Cons(x, Cons(2, Cons(4, _))) ⇒ x
		case Nil ⇒ 42
		case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
		case Cons(h, t) ⇒ h + sum(t)
		case _ ⇒ 101
	}                                         //> x  : Int = 3

	// Exercise 3.2
	def tail[A](ls: List[A]) = ls match {
		case Nil ⇒ Nil
		case Cons(_, h) ⇒ h
	}                                         //> tail: [A](ls: fpinscala.datastructures.List[A])fpinscala.datastructures.List
                                                  //| [A]

	tail(List(1, 2, 3, 4, 5))                 //> res3: fpinscala.datastructures.List[Int] = Cons(2,Cons(3,Cons(4,Cons(5,Nil))
                                                  //| ))
	List(1)                                   //> res4: fpinscala.datastructures.List[Int] = Cons(1,Nil)
	tail(Nil)                                 //> res5: fpinscala.datastructures.List[Nothing] = Nil

	// Exercise 3.3
	def setHead[A](ls: List[A], newVal: A) = Cons(newVal, tail(ls))
                                                  //> setHead: [A](ls: fpinscala.datastructures.List[A], newVal: A)fpinscala.datas
                                                  //| tructures.Cons[A]

	setHead(List(1, 2, 3, 4, 5), 17)          //> res6: fpinscala.datastructures.Cons[Int] = Cons(17,Cons(2,Cons(3,Cons(4,Cons
                                                  //| (5,Nil)))))

	/**
	 * Exercise 3.5
	 *
	 * Generalize tail to the function drop, which removes the first n elements from a list.
	 * Note that this function takes time proportional only to the number of elements being
	 * dropped—we don’t need to make a copy of the entire List.
	 * def drop[A](l: List[A], n: Int): List[A]
	 */

	def drop[A](l: List[A], n: Int): List[A] = {
		if (n == 0) l
		else {
			l match {
				case Cons(_, s) ⇒ drop(s, n - 1)
				case Cons(_, Nil) => Nil
			}
		}

	}                                         //> drop: [A](l: fpinscala.datastructures.List[A], n: Int)fpinscala.datastructu
                                                  //| res.List[A]
	drop(List(1, 2, 3, 4, 5), 2)              //> res7: fpinscala.datastructures.List[Int] = Cons(3,Cons(4,Cons(5,Nil)))
	drop(List(1, Nil), 1)                     //> res8: fpinscala.datastructures.List[Any] = Cons(Nil,Nil)















}