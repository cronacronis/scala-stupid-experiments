package fpinscala.Chapter4

/* https://github.com/fpinscala/fpinscala/tree/master/answerkey */

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
sealed trait Option[+A] {
	/** Apply f if the Option is not None. */

	def map[B](f: A => B): Option[B] = this match {
		case None => None
		case Some(v) => Some(f(v))
	}

	/** Apply f, which may fail, to the Option if not None. */
	def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

	/**  */
	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(v) => v
	}

	/** orElse returns the first Option if it is defined; otherwise, it returns the second Option. */
	def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(x => Some(x)).getOrElse(ob)

	/** Convert Some to None if the value doesn’t satisfy f. */
	def filter(f: A => Boolean): Option[A] = this.map(f) flatMap (valid => if (valid) this else None)
}

object Option {
	def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
	
	/* E. 4.4 */
	private def reduceOption[A](lso: Option[List[A]], candidate: Option[A]): Option[List[A]] = {
		def combine(cand: A): Option[List[A]] = lso map { ls => ls :+ cand }
		val flatMapped: Option[List[A]] = candidate flatMap combine
		flatMapped
	}
	private def makeOptionalList[A]: Option[List[A]] = Some(List[A]())

//	def sequence[A](ls: List[Option[A]]): Option[List[A]] = {
//		ls.foldLeft(makeOptionalList[A])({ case (lso, xo) => reduceOption(lso, xo) })
//	}
	
	/* E. 4.5 */
	def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
		a.foldLeft(makeOptionalList[B])({ case (lso, xo) => reduceOption(lso, f(xo)) })
	}

	def sequence[A](ls: List[Option[A]]): Option[List[A]] = traverse[Option[A], A](ls)(x => x)
	
	/* Ex. 4.3 */
	def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
		a flatMap { aa =>
			b map { bb => f(aa, bb) }
		}
}

object Exercise_4_2 {
	def mean(xs: Seq[Double]): Option[Double] = {
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)
	}

	def variance(xs: Seq[Double]): Option[Double] = {
		val meanVal = mean(xs)
		val subMean = meanVal map (m => xs map { x => math.pow(x - m, 2) })
		subMean flatMap mean
	}
}

trait Either[+E, +A] {

	def Try[A](a: => A): Either[Exception, A] =
		try Right(a)
		catch { case e: Exception => Left(e) }

	def map[B](f: A => B): Either[E, B] = this match {
		case Left(e) => Left(e)
		case Right(value) => Right(f(value))

	}

	/**
	 * When mapping over the right side, we must promote the left type parameter to some
	 * supertype, to satisfy the +E variance annotation.
	 */
	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
		this match {
			case Left(e) => Left(e)
			case Right(v) => f(v)
		}
	}

	def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
		this match {
			case Left(e) => b
			case Right(v) => Right(v)
		}
	}

	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
		for {
			aa <- this
			bb <- b
		} yield f(aa, bb)
	}
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Chapter_4_1 {
	def mean(xs: IndexedSeq[Double]): Either[String, Double] =
		if (xs.isEmpty)
			Left("mean of empty list!")
		else
			Right(xs.sum / xs.length)

	def safeDiv(x: Int, y: Int): Either[Exception, Int] =
		try
			Right(x / y)
		catch { case e: Exception => Left(e) }
}

object Exercise_4_7 {

	//	def reduceEither[E, B](lso: Either[E, List[B]], candidate: Either[E, B]): Either[E, List[B]] = {
	//		def combine(cand: B): Either[E, List[B]] = lso map { ls => ls :+ cand }
	//		val flatMapped: Either[E, List[B]] = candidate flatMap combine
	//		flatMapped
	//	}

	def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

	def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
		//		as.foldLeft(makeEitherList[E, B])({ case (lso, xo) => reduceEither(lso, f(xo)) })
		val emptyEitherList: Either[E, List[B]] = Right(Nil)
		val folded = as.foldLeft(emptyEitherList)(
			{
				case (emptyList, a) =>
					f(a).map2(emptyList)(_ :: _)
			})
		folded
	}

}
sealed class Age(val value: Int)
sealed class Name(val value: String)
case class Person(name: Name, age: Age) {
	override def toString = "Name: " + name.value.toString() + " " + "age: " + age.value.toString()
}


case class LeftExt[+E](value: List[E]) extends Either[E, Nothing] {
	def this(elem: E) = this(List[E](elem))
	override def map2[EE >: E, B, C](newElem: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = {
		newElem match {
			case Right(value) => this
			case LeftExt(ls_e) => LeftExt(this.value ++ ls_e)
			case Left(e) => LeftExt(this.value :+ e)
		}
	}
}

object Exercise_4_8 {
	/**
	 * In this implementation, map2 is only able to report one error, even if both the name
	 * and the age are invalid. What would you need to change in order to report both errors?
	 * 
	 * Would you change map2 or the signature of mkPerson? 
	 * Or could you create a new datatype that captures this requirement better than Either does, with some additionalstructure? 
	 * How would orElse, traverse, and sequence behave differently for that data type?
	 */
	def mkName(name: String): Either[String, Name] =
		if (name == "" || name == null) new LeftExt("Name is empty.")
		else Right(new Name(name))
	def mkAge(age: Int): Either[String, Age] =
		if (age < 0)  new LeftExt("Age is out of range.")
		else Right(new Age(age))
	def mkPerson(name: String, age: Int): Either[String, Person] =
		mkName(name).map2(mkAge(age))(Person(_, _))
}
