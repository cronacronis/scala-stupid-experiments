/*** https://github.com/fpinscala/fpinscala/tree/master/answerkey/laziness */

package fpinscala.Chapter5

import fpinscala.Chapter4.{ None, Option, Some }

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
*/

/*
	Making test covariant in T means that Test[A] is a subtype of Test[Any] for any A. So lets create a Test:
	
	> class Test[+T] {
	>    	var list: List[T] = _
	> }
	
	> val test_string = new Test[String]
	
	Now we have a Test[String] and the contained list is type List[String].
	Since Test[String] is a subtype of Test[Any], the following should be allowed:
	
	> val test_any : Test[Any] = test_string
	
	And now, we have a Test[Any], and therefore test_any.list is type List[Any], 
	which means the following should be valid:
	
	> test_any.list = List[Any]()
	
	Which means we just assigned a List[Any] to test_strings list member, 
	which shouldn't be allowed, since that is supposed to be a List[String], not a List[Any]. 
	It also means you could prepend anything at all to the list, since it is type List[Any].
*/

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

	def cut(n: Int): (Stream[A], Stream[A]) = this match {
		case Empty => { println("hit empty"); (Empty, Empty) }
		case Cons(h, t) if n <= 0 => { println("hit n <= 0"); (Empty, this) }
		case Cons(h, t) => {
			println("hit main cycle")
			lazy val tail = t().cut(n - 1)
			(Stream.cons(h(), tail._1), tail._2)
		}
	}

	/** EXERCISE 5.2 */
	def take(n: Int): Stream[A] = this.cut(n)._1
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

	/* Exer 5.5 */
	def takeWhile2(p: A => Boolean): Stream[A] = this match {
		case Cons(h, t) =>
			{
				lazy val head = h()
				if (p(head)) Stream.cons(head, t().takeWhile2 { p })
				else Empty
			}
		case _ => Empty
	}

	def foldRight[B](z: => B)(f: (A, => B) => B): B = {
		this match {
			case Cons(h, t) => f(h(), t().foldRight(z)(f))
			case _ => z
		}
	}

	def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

	/* Exer 5.4 */
	def forAll(p: A => Boolean): Boolean = {
		def pn(x: A) = !p(x)
		this match {
			case Cons(h, t) if p(h()) == true => t().foldRight(true) { (elem, z) => p(elem) & z }
			case _ => false
		}
	}

	/**
	 * Exercise_5_6
	 *  Hard: Implement headOption using foldRight.
	 */
	def headOption2: Option[A] = foldRight(None: Option[A])(getAsOption)

	def getAsOption[B](elem: B, option: => Option[B]): Option[B] = Some(elem)

	/**
	 * Exercise_5_7
	 * Implement map, filter, append, and flatMap using foldRight. The append method
	 * should be non-strict in its argument.
	 */
	def map[B](f: A => B): Stream[B] = {
		foldRight(Empty: Stream[B]) { (elem, z) => Stream.cons(f(elem), z) }
	}

	def filter(f: A => Boolean): Stream[A] = {
		foldRight(Empty: Stream[A]) {
			(elem, z) =>
				{
					val isCorrect = f(elem)
					if (isCorrect) Stream.cons(elem, z)
					else z
				}
		}
	}

	def append[B >: A](s: => Stream[B]): Stream[B] =
		foldRight(s)((h, t) => Stream.cons(h, t))

	def thisAs[AA >: A]: Stream[AA] = this

	/**
	 * It is not LAZY!
	 */
	def flatMap[B](f: A => Stream[B]): Stream[B] = {
		this.foldRight(Stream.empty[B])((cur, newS) => {
			val fc: Stream[B] = f(cur)
			fc append newS
		})
	}

	/** Exercise 5.13 */
	def map1[B](f: A => B): Stream[B] = {
		Stream.unfold(this)(
			{ s =>
				lazy val mapped = s.headOption.map(f)
				Option.map2(
					mapped,
					Some(s.drop(1)))((_, _))
			}
		)
	}

	def take1(n: Int): Stream[A] = {
		Stream.unfold((this, n))(
			{
				case (s, l) =>
					{
						val mapped = s.headOption
						val newTuple = l match {
							case l if l > 0 => Some((s.drop(1), l - 1))
							case _ => None
						}
						Option.map2(mapped, newTuple)((_, _))
					}
			})
	}

	def takeWhile1(f: A => Boolean): Stream[A] = {
		Stream.unfold(this)(
			{ s =>
				val mapped = s.headOption
				val firstVal = mapped.flatMap(x => f(x) match {
					case true => mapped
					case _ => None
				})
				Option.map2(firstVal, Some(s.drop(1)))((_, _))
			})
	}

	def zipWith[B](s: Stream[B]): Stream[(A, B)] = {
		val init = (this, s)
		Stream.unfold(init)(
			{
				case (cur, s) =>
					val mapped = cur.headOption
					val oMapped = s.headOption
					val merged = Option.map2(mapped, oMapped)((_, _))
					val tail = Some(cur.drop(1), s.drop(1))
					Option.map2(merged, tail)((_, _))
			})
	}

	def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
		val init = (this, s)
		Stream.unfold(init)(
			{
				case (cur, s) =>
					val merged: Option[(Option[A], Option[B])] = (cur.headOption, s.headOption) match {
						case (None, None) => None
						case x => Some(x)
					}
					val tail = Some(cur.drop(1), s.drop(1))
					Option.map2(merged, tail)((_, _))
			})
	}

	def isSubsequence[B](s: Stream[B]): Boolean = {
		(this zipWith s) forAll { case (cur, selem) => cur == selem }
	}

	def asOption: Option[Stream[A]] = {
		this match {
			case Empty => None
			case x => Some(x)
		}
	}

	def dropHead: Option[Stream[A]] = this.drop(1).asOption

	def hasSubsequence[B](s: Stream[B]): Boolean = {
		this.tails exists (_ startsWith s)
	}

	/** Exercise. 5.14 */
	def startsWith[A](s: Stream[A]): Boolean = this.isSubsequence(s)

	/** Exercise. 5.15 */
	def tails: Stream[Stream[A]] = {
		Stream.unfold(this)(cur => Option.map2(cur.asOption, cur.drop(1).asOption)((_, _)))
	}

	def tailsExtended: Stream[Stream[A]] = {
		Stream.unfold(this)(cur => Option.map2(cur.asOption, Some(cur.drop(1)))((_, _)))
	}

	/** Exercise. 5.16 */
	def scanRight[B >: A](z: => B)(f: (A, => B) => B): Stream[B] = {
		//		val comb: Stream[B] = this append Stream(z)
		val tailsSt: Stream[Stream[A]] = this.tails
		val bval = tailsSt.foldRight(Stream.empty[B]) {
			(curSt, aggrSt) =>
				{
					val foldedCur: B = curSt.foldRight(z)(f)
					Stream.cons(foldedCur, aggrSt)
				}
		}
		bval
	}

	//	def all[A](a: Option[A], b: Option[A]): Option[A] = (a,b) match {
	//		case (None, None) => None
	//		case (None, x) => x
	//		case (y, None) => y
	//	}

	def scanRight2[B](z: => B)(f: (A, => B) => B): Stream[B] = {
		val tailsSt: Stream[Stream[A]] = this.tailsExtended
		val bval = tailsSt.foldRight(Stream.empty[B]) {
			(curSt, aggrSt) =>
				{
					val foldedCur: B = curSt.foldRight(z)(f)
					Stream.cons(foldedCur, aggrSt)
				}
		}
		bval.append(Stream(z))
	}
	
	def scanRight3[B](z: B)(f: (A, => B) => B): Stream[B] = {
		val folded = foldRight((z, Stream(z))){
			case(curV, (zval, aggrSt)) =>
				{
					val newVal: B = f(curV, zval)
					(newVal , Stream.cons(newVal, aggrSt))
				}
		}
		folded._2
	}
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

	def constant[A](a: A): Stream[A] = Stream.cons(a, Stream.constant(a))

	/*E. 5.9*/
	def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n + 1))

	/*E. 5.10*/
	def trailingFib(prevTwo: Stream[Int]): Stream[Int] = {
		val lsprevTwo = prevTwo.toList
		lazy val lastTwo = lsprevTwo.sum
		Stream.cons(lastTwo, trailingFib(Stream(lsprevTwo.last, lastTwo)))
	}
	def fibs: Stream[Int] = {
		val init: Stream[Int] = Stream(0, 1, 1)
		val fibsVals = init append trailingFib(init)
		fibsVals
	}

	/*E. 5.11*/
	/**
	 * It takes an initial state, and a function for producing both the next state and
	 * the next value in the generated stream. Option is used to indicate when the Stream should be terminated, if at all.
	 * The function unfold is a very general Stream-building function.
	 *
	 * The unfold function is productive as long as f terminates, since we just need to run the
	 * function f one more time to generate the next element of the Stream.
	 *
	 */
	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
		f(z) match {
			case None => Stream.empty[A]
			case Some(v) => Stream.cons(v._1, unfold(v._2)(f))
		}
	}

	/*E. 5.12 */
	def fibs2: Stream[Int] = {
		val init = Stream(0, 1)
		init append Stream.unfold(init)(st =>
			{
				val ls = st.toList
				val newVal = ls.sum
				Some(newVal, Stream(ls.last, newVal))
			})
	}

	def from2(n: Int): Stream[Int] = {
		Stream(n) append Stream.unfold(n)(intX =>
			{
				val newVal = intX + 1
				Some(newVal, newVal)
			})
	}

	def constant2[A](a: A): Stream[A] = Stream.unfold(a)(x => Some(x, x))

}

