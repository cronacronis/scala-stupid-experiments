package Week7

import G.{ hasNewVol, isFound }
import G.{fillToEmpty, noneToEmpty, fillToNone, noneToNone}


object recursivePour {
  type Glasses = Map[String, Gls]
  val caps: Vector[Double] = Vector(4, 9)
  val gs = caps.map(Gls.mkLb).toMap
  ////////////////////////////////////////////////////////////////////////////
  val src = gs("4")
  val dst = gs("9")
  Tap.vol
  src.Filler
  dst.Filler
  ////////////////////////////////////////////////////////////////////////
  def pour(gs: Glasses)(src: Gls, dst: Gls)(f: (Gls, Gls) => Gls): Glasses = {
    val ndst = f(src, dst)
    Vector(ndst, ndst.Filler).foldLeft(gs)({ case (gs, g) => gs + Gls.mkGlsTup(g) })
  }
  pour(gs = gs)(src = dst, dst = dst)(f = fillToNone())
  ////////////////////////////////////////////////////////////////////////
  
  val target = 6.0
  
  val funs = Vector(fillToEmpty() _ , noneToEmpty () _ , fillToNone() _ , noneToNone()  _)

  def simulate(gs: Glasses, target: Double): Vector[Glasses] = {
    if (isFound(gs, target))
      Vector(gs)
    else {

      val simVec = (for {
        src <- gs.values
        dst <- gs.values
        f <- funs
        if src.lab != dst.lab
        val remap = pour(gs)(src, dst)(f)
        if hasNewVol(gs, remap)
      } yield remap) toVector
		
      //println(simVec)
      simVec flatMap (gsx => simulate(gsx, target)) take 1
    }
  
  }
  
  //gs
  
  val success = simulate(gs, target)
	success(0)("9").History
  ////////////////////////////////////////////////////////////////////////////////
  /** Fun: gls => Vector[Gls] */
}