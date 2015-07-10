package fpinscala.Chapter5

object SomeFunctions {
	def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0
	//maybeTwice: (b: Boolean, i: => Int)Int

	def maybeTwice2(b: Boolean, i: => Int) = {
		lazy val j = i
		if (b) j + j else 0
	}
}

/*
	Variadic functions are just providing a little syntactic sugar for creating and passing
	a Seq of elements explicitly. Seq is the interface in Scala’s collections library imple-
	mented by sequence-like data structures such as lists, queues, and vectors. Inside
	apply, the argument as will be bound to a Seq[A] (documentation at http://mng.bz/
	f4k9), which has the functions head (returns the first element) and tail (returns all
	elements but the first). The special _* type annotation allows us to pass a Seq to a
	variadic method.
 * */

/**
 * A nonempty stream consists of a head and a tail,
 * which are both non-strict. Due to technical
 * limitations, these are thunks that must be
 * explicitly orced, rather than by-name parameters.
 */

sealed trait Stream[+A] {
	def headOption: Option[A] = this match {
		case Empty => None
		case Cons(h, t) => Some(h())
	}

	/** Exercise_5_1 */
	def toList: List[A] = this match {
		case Empty => Nil
		case Cons(h, t) => h() :: t().toList
	}

	def cut(n: Int): (List[A], Stream[A]) = this match {
		case Empty => { println("hit empty"); (Nil, this) }
		//		case Empty => throw new IllegalStateException("Exception thrown"); 
		case Cons(h, t) if n <= 0 => { println("hit n"); (Nil, this) }
		case Cons(h, t) => {
			println("hit main cycle")
			lazy val tail = t().cut(n - 1)
			(h() :: tail._1, tail._2)
		}
	}
	/** EXERCISE 5.2 */
	def take(n: Int): List[A] = this.cut(n)._1
	/** EXERCISE 5.2 */
	def drop(n: Int): Stream[A] = this.cut(n)._2

	def cutWhile(p: A => Boolean): (List[A], Stream[A]) = this match {
		case Empty => (Nil, this)
		case Cons(h, t) =>
			{
				val toCut = p(h())
				if (toCut) {
					lazy val tail = t().cutWhile(p)
					(h() :: tail._1, tail._2)
				} else {
					(Nil, this)
				}
			}
	}
	
	def takeWhile(p: A => Boolean): Stream[A] = cutWhile(p)._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

	/**
	 * A smart constructor for creating an empty stream of a particular type.
	 *  By convention, smart constructors typically lowercase the first letter of the
	 *  corresponding data constructor.
	 */
	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		/**We cache the head and tail as lazy values to avoid repeated evaluation.*/
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)

	}

	def empty[A]: Stream[A] = Empty

	/** A convenient variable-argument method for constructing a Stream from multiple elements. */
	def apply[A](as: A*): Stream[A] = {
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))
	}
}

