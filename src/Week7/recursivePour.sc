package Week7

import G.{ hasNewVol, isFound, allVols, pour, simulate }
import G.{ fillToEmpty, noneToEmpty, fillToNone, noneToNone }

object recursivePour {
  type Glasses = Map[String, Gls]
  val caps: Vector[Double] = Vector(4, 9)         //> caps  : Vector[Double] = Vector(4.0, 9.0)
  
  val gs = caps.map(Gls.mkLb).toMap               //> 4.0
                                                  //| 9.0
                                                  //| gs  : scala.collection.immutable.Map[String,Week7.Gls] = Map(4 -> [0.0 / 4.0
                                                  //| ], 9 -> [0.0 / 9.0])
  ////////////////////////////////////////////////////////////////////////////
  val src = gs("4")                               //> src  : Week7.Gls = [0.0 / 4.0]
  val dst = gs("9")                               //> dst  : Week7.Gls = [0.0 / 9.0]
  Tap.vol                                         //> res0: Double = 1.0E10
  ////////////////////////////////////////////////////////////////////////
  pour(gs = gs)(src = dst, dst = dst)(f = fillToNone())
                                                  //> res1: Week7.G.Glasses = Map(4 -> [0.0 / 4.0], 9 -> [0.0 / 9.0])
  //pour(gs = gs)(src = dst, dst = dst)(f = fillToNone)
  ////////////////////////////////////////////////////////////////////////

  val target = 6.0                                //> target  : Double = 6.0

  val success = simulate(gs, target)              //> success  : Vector[Week7.G.Glasses] = Vector(Map(4 -> [0.0 / 4.0], 9 -> [6.0 
                                                  //| / 9.0]))

  success(0)("9").PrintHistory mkString "\n"      //> res2: String = List([6.0 / 9.0])
                                                  //| List([2.0 / 9.0], [0.0 / 4.0])
                                                  //| List([2.0 / 9.0], [0.0 / 4.0])
  
  /*success(0)("9").History
  success(0)("9").Filler
  success(0)("9").prevVols
  success(0)("9").Filler.get.Filler */
  ////////////////////////////////////////////////////////////////////////////////
}