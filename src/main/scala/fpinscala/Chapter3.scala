package fpinscala

import org.omg.CORBA.Object

//sealed trait List[+A]
//case object Nil extends List[Nothing]
//case class Cons[+A](head: A, tail: List[A]) extends List[A]
//object List {
//	def sum(ints: List[Int]): Int = ints match {
//		case Nil =>0
//		case Cons(x, xs) =>x + sum(xs)
//	}
//	def product(ds: List[Double]): Double = ds match {
//		case Nil =>1.0
//		case Cons(0.0, _) =>0.0
//		case Cons(x, xs) =>x * product(xs)
//	}
//	def apply[A](as: A*): List[A] =
//		if (as.isEmpty) Nil
//		else Cons(as.head, apply(as.tail: _*))
//}

object Exercise_3_24 {

	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
		(sup, sub) match {
			case (Nil, Nil) => false
			case (sup, Nil) => false
			case (Nil, sub) => false
			case (sup, sub) if sup.size < sub.size => false
			case (hd :: tl, _) => {
				val zipped = sub zip sup.take(sub.size)
				val equal = zipped takeWhile ({ case (subv, supv) => subv == supv })
				if (equal.size == sub.size) { true } else { hasSubsequence(tl, sub) }
			}
		}
	}
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Exercise_3_25 {
	def sizeTree[A](tr: Tree[A], nleaves: Int = 0, nbranches: Int = 0): (Int, Int) =
		{
			tr match {
				case Leaf(v) => (nleaves + 1, nbranches)
				case Branch(r, l) => {
					val leftSize = sizeTree(r, nleaves, nbranches + 1)
					val rightSize = sizeTree(l, nleaves, nbranches)
					(leftSize, rightSize) match { case ((l_leaves, l_branches), (r_leaves, r_branches)) => ((l_leaves + r_leaves), (l_branches + r_branches)) }
				}
			}
		}
}









