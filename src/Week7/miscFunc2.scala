package Week7

object G {
  type Glasses = Map[String, Gls]

  /** Might be a need that all of glasses simultaneosly been with vols*/
  def hasNewVol(prior: Glasses, post: Glasses): Boolean = {
    val states = for {
      og <- prior.values
      ng <- post.values
      val isNew = (og IsNewState ng)
      if (isNew)
    } yield (og IsNewState ng)
    states exists { s => s == true }
  }

  def allVols(gs: Glasses): Vector[Double] =
    ((gs flatMap { case (asMap, g) => Vector(g.vol, g.maxVol) } toVector) ++ Vector(0.0)) distinct

  def hasSameVols(gs: Glasses, gs2: Glasses): Boolean = {
	  allVols(gs) sameElements allVols(gs2)
  }
    
  def glasessToMap(gls: Iterable[Gls]) = {
    (gls foldLeft Map[String, Gls]()) { case (asMap, g) => asMap + Gls.mkGlsTup(g) }
  }

  def isFound(gs: Glasses, target: Double): Boolean = {
    allVols(gs).contains(target)
  }

  def fillToEmpty()(src: Gls, dst: Gls): Gls = {
    src.FillFull PourTo dst.Empty
  }

  def noneToEmpty()(src: Gls, dst: Gls): Gls = {
    src PourTo dst.Empty
  }

  def fillToNone()(src: Gls, dst: Gls): Gls = {
    src.FillFull PourTo dst
  }

  def noneToNone()(src: Gls, dst: Gls): Gls = {
    src PourTo dst
  }
  
  def pour(gs: Glasses)(src: Gls, dst: Gls)(f: (Gls, Gls) => Gls): Glasses = {
    val ndst = f(src, dst)
    ndst.WithFiller.foldLeft(gs)({ case (gs, g) => gs + Gls.mkGlsTup(g) })
  }  
  
  def simulate(gs: Glasses, target: Double): Vector[Glasses] = {
    val funs = Vector(fillToEmpty() _, noneToEmpty() _, fillToNone() _, noneToNone() _)
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
      val grouped = simVec groupBy(gs => allVols(gs))
      val unique = grouped.map{case(m, gsV) => gsV head}.toVector
      unique flatMap (gsx => simulate(gsx, target) ) take 1
      //unique map (gsx => simulate(gsx, target) :+ gsx )
      
    }

  }

}