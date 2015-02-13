package Week6

object ForQueries {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	
	val books: Set[Book] = Set(
		Book("Structure and Interpretation of Computer Programs",
			List("Abelson, Harold", "Sussman, Gerald J.")),
		Book("Principles of Compiler Design", List("Aho, Alfred", "Ullman, Jeffrey")),
		Book("Programming in Modula-2", List("Wirth, Niklaus")),
		Book("Introduction to Functional Programming", List("Bird, Richard")),
		Book("The Java Language Specification",
			List("Gosling, James", "Joy, Bill", "Steele, Guy", "Bracha, Gilad")),
		Book("Java puzzlers", List("Bloch, Joshua", "Gafter, Neal")),
		Book("Effective Java", List("Bloch, Joshua"))
	)                                         //> books  : Set[Week6.Book] = Set(Book(Java puzzlers,List(Bloch, Joshua, Gafter
                                                  //| , Neal)), Book(Structure and Interpretation of Computer Programs,List(Abelso
                                                  //| n, Harold, Sussman, Gerald J.)), Book(Programming in Modula-2,List(Wirth, Ni
                                                  //| klaus)), Book(Effective Java,List(Bloch, Joshua)), Book(The Java Language Sp
                                                  //| ecification,List(Gosling, James, Joy, Bill, Steele, Guy, Bracha, Gilad)), Bo
                                                  //| ok(Principles of Compiler Design,List(Aho, Alfred, Ullman, Jeffrey)), Book(I
                                                  //| ntroduction to Functional Programming,List(Bird, Richard)))
	for (b <- books; a <- b.authors if a startsWith "Bird")
	yield b.title                             //> res0: scala.collection.immutable.Set[String] = Set(Introduction to Functiona
                                                  //| l Programming)
	
	for (b <- books if b.title.indexOf("Program") >= 0)
	yield b.title                             //> res1: scala.collection.immutable.Set[String] = Set(Structure and Interpretat
                                                  //| ion of Computer Programs, Programming in Modula-2, Introduction to Functiona
                                                  //| l Programming)
	for {
		b1 <- books
		b2 <- books
		if b1 != b2
		a1 <- b1.authors
		a2 <- b2.authors
		if a1 == a2
	} yield a1                                //> res2: scala.collection.immutable.Set[String] = Set(Bloch, Joshua)
 

}



case class Book(title: String, authors: List[String])