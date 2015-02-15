package Week7

import Week7.F._

object waterPouring {

	def test(a: Int): Int = {
		if(a == 0) a + 1 else a + 2
		
	}                                         //> test: (a: Int)Int
	
	
	Vector(5, 3, 2) contains 5                //> res0: Boolean = true
	test(0)                                   //> res1: Int = 1

  val A = Gls(lab = "A", _vol = 4, maxVol = 5, filler = null)
                                                  //> A  : Week7.Gls = [4.0 / 5.0]
  val b = Gls(lab = "b", _vol = 2, maxVol = 3, filler = null)
                                                  //> b  : Week7.Gls = [2.0 / 3.0]
  //A pourTo b pourTo A pourTo b
 
  val dstAmount = 6                               //> dstAmount  : Int = 6
  val caps: Vector[Double] = Vector(4, 9)         //> caps  : Vector[Double] = Vector(4.0, 9.0)
  val glasses = caps.map(Gls.mkLb).toMap          //> glasses  : scala.collection.immutable.Map[String,Week7.Gls] = Map(4 -> [0.0 
                                                  //| / 4.0], 9 -> [0.0 / 9.0])
  
  val glassesHalf: Glasses = Map( "2" -> new Gls("4", 1, 2, null), "2" -> new Gls("3", 1, 3, null))
                                                  //> glassesHalf  : Week7.F.Glasses = Map(2 -> [1.0 / 3.0])
  
  //val totalCap = { for (glass <- glasses.values) yield glass.maxVol }.sum
 	allVols(glasses)                          //> res2: Vector[Double] = Vector(0.0, 4.0, 9.0)
 	
 	
 	isNew(Gls("test", 1 , 2, null), glasses)  //> res3: Boolean = true
 	
	repourToGenNew(glasses)                   //> res4: Week7.F.Glasses = Map(9 -> [4.0 / 9.0], 4 -> [4.0 / 4.0])

  //repour(glasses)
  //repour(glassesHalf)
  
  
  keepPouring(glasses, dstAmount)                 //> Map(9 -> [4.0 / 9.0], 4 -> [4.0 / 4.0])
                                                  //| Map(9 -> [4.0 / 9.0], 4 -> [4.0 / 4.0])
                                                  //| ////
                                                  //| Map(9 -> [8.0 / 9.0])
                                                  //| Map(9 -> [8.0 / 9.0])
                                                  //| ////
                                                  //| Map()
                                                  //| Map()
                                                  //| ////
                                                  //| Map()
                                                  //| java.lang.RuntimeException: Dead end reached
                                                  //| 	at scala.sys.package$.error(package.scala:27)
                                                  //| 	at scala.Predef$.error(Predef.scala:142)
                                                  //| 	at Week7.F$.keepPouring(miscFunc.scala:88)
                                                  //| 	at Week7.waterPouring$$anonfun$main$1.apply$mcV$sp(Week7.waterPouring.sc
                                                  //| ala:38)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at Week7.waterPouring$.main(Week7.waterPouring.scala:5)
                                                  //| 	at Week7.waterPouring.main(Week7.waterPouring.scala)
  //glasses ++ repour(glasses)
  //repour(repour(repour(glasses)))
  //repour(glasses.values).filter(p => p.isSemiFull).flatMap(g => List(g.filler.getOrElse(Tap), g))
  //repour(glasses.values).filter(p => p.isSemiFull).flatMap(g => history(g))

 // history(repour(glasses).head)

  //repour(repour(glasses.values)).filter(p => p.isSemiFull).flatMap(g => List(g.filler.getOrElse(Tap), g))
  
	
}