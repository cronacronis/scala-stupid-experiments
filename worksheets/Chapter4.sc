//package fpinscala
import fpinscala.Chapter3.Exercise_3_24._

import fpinscala.Chapter3.Exercise_3_25._
import fpinscala.Chapter4._

object SubList {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  /*
  hasSubsequence(List(1,2,3), List(1,2))
  hasSubsequence(List(1,2,3), List(1))
  hasSubsequence(List(1,2,3), List(2,3))
  hasSubsequence(List(1,2,3), List(1,2,5))

	val tr = Branch(Leaf(5), Branch(Leaf(2), Branch(Leaf(2), Branch(Leaf(2), Leaf(7)))))
	sizeTree(tr)    */


	//val listOfOptions = List(Some(5))
	//val listOfOptions: List[Option[Int]] = List(Some(5), None)
	val listOfOptions: List[Option[Int]] = List(Some(5), Some(3), None)
                                                  //> listOfOptions  : List[fpinscala.Chapter4.Option[Int]] = List(Some(5), Some(3
                                                  //| ), None)
	Exercise_4_4.sequence(listOfOptions)      //> res0: fpinscala.Chapter4.Option[List[Int]] = None
	
	Exercise_4_5.sequence(List(Some(5), Some(3)))
                                                  //> res1: fpinscala.Chapter4.Option[List[Int]] = Some(List(5, 3))
	List(1) :+ 2                              //> res2: List[Int] = List(1, 2)

	List(1) ++ List(2)                        //> res3: List[Int] = List(1, 2)

	Exercise_4_4.reduceOption(Some(List(1)), Some(2))
                                                  //> res4: fpinscala.Chapter4.Option[List[Int]] = Some(List(1, 2))
	Exercise_4_4.reduceOption(Some(List()), Some(2))
                                                  //> res5: fpinscala.Chapter4.Option[List[Int]] = Some(List(2))
	Exercise_4_4.reduceOption(Some(List(1, 2)), None)
                                                  //> res6: fpinscala.Chapter4.Option[List[Int]] = None
	Exercise_4_4.reduceOption(Some(List()), None)
                                                  //> res7: fpinscala.Chapter4.Option[List[Nothing]] = None

	val felipe = Person( new Name("Felipe"), new Age(28))
                                                  //> felipe  : fpinscala.Chapter4.Person = Name: Felipe age: 28
	 
	val noname = Person( new Name(""), new Age(28))
                                                  //> noname  : fpinscala.Chapter4.Person = Name:  age: 28
	val noage = Person( new Name("Lololo"), new Age(-1))
                                                  //> noage  : fpinscala.Chapter4.Person = Name: Lololo age: -1

	Exercise_4_8.mkPerson("test", 5)          //> res8: fpinscala.Chapter4.Either[String,fpinscala.Chapter4.Person] = Right(N
                                                  //| ame: test age: 5)
	//Exercise_4_8.mkPerson("", 5)
	val twoErrors = Exercise_4_8.mkPerson("", -1)
                                                  //> twoErrors  : fpinscala.Chapter4.Either[String,fpinscala.Chapter4.Person] = 
                                                  //| LeftExt(List(Name is empty., Age is out of range.))
	
}