
package fpinscala.Chapter6

import fpinscala.Chapter5._
import fpinscala.Chapter4._
import scala.Nothing

/** https://github.com/cronacronis/fpinscala/tree/master/answerkey/state */

trait RNG {
	def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

	import rNG._

	def nextInt: (Int, RNG) = {
		val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
		val nextRNG = SimpleRNG(newSeed)
		val n = (newSeed >>> 16).toInt
		(n, nextRNG)
	}

	def nonNegativeInt(rng: RNG): (Int, RNG) = {
		val (i, r) = rng.nextInt
		val rv =
			if (i < 0) -(i + 1)
			else i
		(rv, r)
	}

	/** EXERCISE 6.2 */
	def double01(rng: RNG): (Double, RNG) = {
		val (i, r) = nonNegativeInt(rng)
		(i.toDouble / Int.MaxValue.toDouble, r)
	}

	/** EXERCISE 6.3 */
	def intDouble(rng: RNG): ((Int, Double), RNG) = {
		val (i, r1) = nextInt
		val (d, r2) = double01(r1)
		((i, d), r2)
	}

	def doubleInt(rng: RNG): ((Double, Int), RNG) = {
		intDouble(rng) match {
			case ((i, d), r) => ((d, i), r)
		}
	}

	/** EXERCISE 6.4 */
	def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
		val newVals = Stream.unfold(rng) {
			//f: S => Option[(A, S)]
			initGen =>
				{
					val generated = initGen.nextInt
					Option.map2(Some(generated), Some(generated._2))((_, _))
				}
		}
		val genStream = newVals take (count)
		genStream.foldRight(List[Int](), rng) {
			case ((curInt, curRng), (nlist, prevRng)) => {
				((curInt :: nlist), curRng)
			}

		}
	}
	/** My own generalization */
	//	def randGen[B](rng: RNG)(f: RNG => (B, RNG)): Stream[(B, RNG)] = {
	//		Stream.unfold(rng) {
	//			//f: S => Option[(A, S)]
	//			initGen =>
	//				{
	//					val generated = f(initGen)
	//					Option.map2(Some(generated), Some(generated._2))((_, _))
	//				}
	//		}
	//	}
	//	def folderToList[B](str: Stream[(B, RNG)]): (List[B], RNG) = {
	//		//todo make foldLeft
	//		val vals = str.foldRight((List[B](), List[RNG]())) {
	//			case ((curInt, curRng), (nlist, prevRngList)) => {
	//				(curInt :: nlist, curRng :: prevRngList)
	//			}
	//		}
	//		(vals._1, vals._2.last)
	//	}

	//	def intStream(rng: RNG) = randGen(rng)(x => nextInt)
	//	def doubleStream(rng: RNG) = randGen(rng)(x => double(x))
	//	def ints4(count: Int)(rng: RNG): (List[Int], RNG) = {
	//		folderToList(intStream(rng).take(count))
	//
	//	}
	//	def double3(rng: RNG): ((Double, Double, Double), RNG) = {
	//		val newVals = Stream.unfold(rng) {
	//			//f: S => Option[(A, S)]
	//			initGen =>
	//				{
	//					val generated = double(initGen)
	//					Option.map2(Some(generated), Some(generated._2))((_, _))
	//				}
	//		}
	//		val genList: List[(Double, RNG)] = newVals take 3 toList
	//		val toRet = ((genList(0)._1, genList(1)._1, genList(2)._1), genList.last._2)
	//		toRet
	//	}
	/********************************************/
//	type Rand[+A] = RNG => (A, RNG)
//	type Rand[A] = State[RNG, A]

	val int: Rand[Int] = _.nextInt
	
	
	def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - (i % 2))

	/** EXERCISE 6.5 */
	def double: Rand[Double] = {
		map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)
	}

	val randIntDouble: Rand[(Int, Double)] = both(int, double)

	val randDoubleInt: Rand[(Double, Int)] = both(double, int)

	def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
		val (i, rng2) = nonNegativeInt(rng)
		val mod = i % n
		if (i + (n - 1) - mod >= 0)
			(mod, rng2)
		else
			nonNegativeLessThan(n)(rng)
	}
	def nonNegativeLessThan2(x: Int): Rand[Int] = {
		flatMap(nonNegativeInt)(
			z => {
				rng =>
					{
						val mod: Int = z % x
						if (z + (x - 1) - mod >= 0)
							(mod, rng)
						else
							nonNegativeLessThan2(z)(rng)
					}
			})
	}

	def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}



object rNG {
	type Rand[+A] = RNG => (A, RNG)
//	type Rand[A] = State[RNG, A]

	def unit[A](a: A): Rand[A] = rng => (a, rng)

	def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
		rng => {
			val (a, rng2) = s(rng) //generate new value and generator
			(f(a), rng2) // passed mapped value and new generator
		}
	/**
	 * EXERCISE 6.6
	 *  Write the implementation of map2 based on the following signature.
	 *  This function takes two actions, ra and rb, and a function f
	 *  for combining their results, and
	 *  returns a new action that combines them:
	 */

	def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
		rng => {
			val (a, rng2) = ra(rng) // a: A
			val (b, rng3) = rb(rng2) // b: B
			(f(a, b), rng3)
		}

	def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

	/** EXERCISE 6.7 */
	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
		val emp = unit(List[A]())
		fs.foldRight(emp) {
			(r, aggr) => map2(r, aggr)((x, aggr) => x :: aggr)
		}
	}

	/** EXERCISE 6.8 */
	def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
		val (a, rng2): (A, RNG) = f(rng)
		val b: Rand[B] = g(a)
		val c: (B, RNG) = b(rng2)
		c
	}
	/** EXERCISE 6.9 */
	def map_1[A, B](s: Rand[A])(f: A => B): Rand[B] = {
		flatMap(s)(x => unit(f(x)))
	}

	def map2_1[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
		flatMap(ra) { raa => map_1(rb: Rand[B]) { rbb => f(raa, rbb) } }
	}
}

case class State[S, +A](run: S => (A, S)) {
	//type State[S, +A] = S => (A, S)

	/** EXERCISE 6.10 */
	def flatMap[B](g: A => State[S, B]): State[S, B] = State(
		(rng: S) => {
			val (a, rng2): (A, S) = this.run(rng)
			val b: State[S, B] = g(a)
			val c: (B, S) = b.run(rng2)
			c
		}
	)

	def map[B](f: A => B): State[S, B] = {
		this.flatMap { a => State.unit(f(a)) }
	}

	def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = {
		//		this.flatMap { x => rb.map }
		this.flatMap { raa => rb.map(rbb => f(raa, rbb)) }
	}
	
		
}

object State{
	
	def unit[A, S](a: A): State[S, A] = State(st => (a, st))
	
	def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
		val emp = State.unit[List[A], S](List[A]())
		fs.foldRight(emp) {
			(r, aggr) => r.map2(aggr)((x, aggr) => x :: aggr)
		}
	}
}



















