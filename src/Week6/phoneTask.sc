package Week6
import scala.io.Source
object phoneTask {
  println("Welcome to the Scala worksheet")
	
	val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
	val words = in.getLines.toList filter (word => word forall ( chr => chr.isLetter ))
	//val words = List[String]("Ababa", "aback", "Ababb")
	val mnem = Map( '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
	val charCode: Map[Char, Char] = (for{ (digit, str) <- mnem; ltr <- str } yield (ltr -> digit))
  def GetCharCode(ltr: Char): Char = charCode(ltr.toUpper)
	//def wordCode(word: String): String = for{ ltr <- word } yield charCode(ltr.toUpper)
	def wordCode(word: String): String = word map GetCharCode
	wordCode("Java")
	
	/*def append(m: Map[String, Seq[String]], c: String, w: String): Map[String, Seq[String]] = {
		val default: Seq[String] = Seq[String]()
		val curValueApp:  Seq[String] = m.getOrElse(c, default) :+ w
		m + (c -> curValueApp)
	}
	val wordsForNum: Map[String, Seq[String]] =
		words.foldLeft(Map[String, Seq[String]]())
		{case(nm, word) => {
				val code = wordCode(word)
				//println(code + " " + word)
				append(nm, code, word) }
		} */
		
	val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()
	
	def encode(number: String): Set[List[String]] =
		if (number.isEmpty) Set(List())
		else
		{
			for
			{
				split <- 1 to number.length
				word <- wordsForNum(number take split)
				rest <- encode(number drop split)
			} yield word :: rest
		}.toSet
	//encode("7225247386")
   def translate(number: String): Set[String] =
   	encode(number) map (_ mkString " ")

	translate("7225247386")
}