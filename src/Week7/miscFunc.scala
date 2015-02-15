package Week7

object F {

  type Glasses = Map[String, Gls]

  def glasessToMap(gls: Iterable[Gls]) = {
    (gls foldLeft Map[String, Gls]()) { case (asMap, g) => asMap + Gls.mkGlsTup(g) }
  }

  def semiFullVols(gs: Glasses): Vector[Double] =
    gs filter { case (lab, g) => g.isSemiFull } map { case (lab, g) => g.vol } toVector

  def allVols(gs: Glasses): Vector[Double] =
    ((gs flatMap { case (asMap, g) => Vector(g.vol, g.maxVol) } toVector) ++ Vector(0.0)) distinct

  def history(lg: (String, Gls)) = lg._2.History

  def isNew(g: Gls, prior: Glasses): Boolean = {
    if ((!g.isSemiFull) || prior.getOrElse(g.lab, g) == g) false
    //val vols = allVols(prior)
    val vols = semiFullVols(prior)
    !vols.contains(g.vol)
  }

  /**
   * If no new capacity generated -> it is deadEnd
   */
  def isDeadEnd(prior: Glasses, post: Glasses): Boolean = {
    val priorV = semiFullVols(prior)
    val postV = semiFullVols(post)
    priorV.sameElements(postV)
  }

  def isFound(prior: Glasses, target: Double): Boolean = {
    allVols(prior).contains(target)
  }

  /**
   *
   */

  /**
   * Pour if it will generate new volume, otherwise empty try again, full try again
   */
  //def refillEmpty(gs: Glasses): Glasses = gs map { case (asMap, g) => if (g.isEmpty) (asMap -> g.FillFull) else (asMap -> g)}
  //def emptyFull(gs: Glasses): Glasses = gs map { case (asMap, g) => if (g.isFull) (asMap -> g.empty) else (asMap -> g) }
  def repourToGenNew(gs: Glasses): Glasses = {
    val glsSet = (
      for {
        src <- gs.values
        dst <- gs.values

        if src.lab != dst.lab

        val filler = if (src.isEmpty) src.FillFull else src
        val newDst = filler PourTo dst

        if isNew(newDst, gs)
      } yield newDst) toSet /*TODO Add source glass here as well if isNew*/

    glasessToMap(glsSet)
  }

  /**
   * Glasses are being repoured to each other
   */
  def repour(gs: Glasses): Glasses = {

    val gsEmptied = gs map { case (asMap, g) => if (g.isFull) g.Empty else g }
    val areAllEmpty: Boolean = gsEmptied filterNot { g => g.isEmpty } isEmpty
    val glsSet = (for {
      src <- gsEmptied //gs.values
      dst <- gsEmptied //gs.values
      if src.lab != dst.lab
      val filler = if (areAllEmpty) src.FillFull else src
    } yield (filler PourTo dst)) toSet

    glasessToMap(glsSet)
  }

  def keepPouring(gs: Glasses, dstAmount: Double): Glasses = {
    if (isFound(gs, dstAmount)) gs
    val post = repourToGenNew(gs)

    val print = println(post)

    if (isDeadEnd(gs, post)) error("Dead end reached")
    else { println(post); println("////"); keepPouring(post, dstAmount) }
  }
}