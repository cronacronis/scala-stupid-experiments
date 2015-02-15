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

  private def allVols(gs: Glasses): Vector[Double] =
    ((gs flatMap { case (asMap, g) => Vector(g.vol, g.maxVol) } toVector) ++ Vector(0.0)) distinct

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

}