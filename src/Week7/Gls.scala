package Week7

object Gls {
  def mkLb(cap: Double): (String, Gls) = {
    val lab = cap.toInt.toString
    (lab -> Gls.mk(lab, 0, cap, None, zeroState))
  }

  def tap: Gls = Tap

  def zeroState = Vector[Double](0.0)

  def mkGlsTup(g: Gls) = (g.lab -> g)

  def mk(lab: String, vol: Double, maxVol: Double, filler: Option[Gls], prevVols: Vector[Double]): Gls = {
    new Gls(lab, vol, maxVol, filler, prevVols)
  }

}

object Tap extends Gls("Tap", 1e10, 1e20, None, Vector[Double](1e10)) {
  override def FillFull = Tap
  override def Empty = Tap
  override def PourTo(dst: Gls) = Tap
  override def toString = lab + "_[+/+]"
}
/**
 * Class to represent Glass
 * @param lab
 * @param prevVols contains previous volumes for a glass
 * @param filler glass which been used to fill current
 */
class Gls(val lab: String, val maxVol: Double, _vol: Double, filler: Option[Gls], prevVols0: Vector[Double]) {
  /* Body */
  private val src = this
  val vol = if (_vol > maxVol) maxVol else _vol
  val avail: Double = maxVol - vol
  val isEmpty = vol == 0.0
  val isFull = vol == maxVol
  val isSemiFull = !isEmpty && !isFull
  val prevVols = prevVols0 :+ vol

  override def toString =
    /*lab + "_" + */ "[" + vol + " / " + maxVol + "]" //+":" + math.round(vol / maxVol * 100) + "%"

  private def _afterPouring(dst: Gls): (Double, Double) = {
    val transportable = math.min(src.vol, dst.avail)
    (vol - transportable, dst.vol + transportable)
  }

  def FillFull = Gls.mk(this.lab, this.maxVol, this.maxVol, Some(Tap), Gls.zeroState)

  def Empty = Gls.mk(lab, 0.0, maxVol, None, this.prevVols)

  def PourTo(dst: Gls): Gls = {
    if (src.isEmpty) {
      dst
    } else if (dst.lab == src.lab) {
      dst
    } else {
      val (srcVol, dstVol) = _afterPouring(dst)
      val srcState = Gls.mk(src.lab, srcVol, src.maxVol, src.Filler, src.prevVols)
      val dstState = Gls.mk(dst.lab, dstVol, dst.maxVol, Some(srcState), dst.prevVols)
      dstState
    }
  }

  def IsNewState(newGlass: Gls): Boolean = {
    if (newGlass.lab != this.lab) {
      false
    } else {
      !(this.prevVols.contains(newGlass.vol))
    }
  }

  /**
   * @return source glasses, chain until reach Tap
   */
  def History: List[Gls] = this.Filler match {
    case null => List()
    case None => List()
    case Some(Tap) => List()
    case _ => this :: this.Filler.get.History
  }

  def Filler: Option[Gls] = filler //.getOrElse(Tap)
}

















