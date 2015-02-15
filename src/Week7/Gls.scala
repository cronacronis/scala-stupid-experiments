package Week7

object Gls {
  def mkLb(cap: Double): (String, Gls) = {
    println(cap)
    val lab = cap.toInt.toString
    (lab -> Gls.mk(lab = lab, vol = 0, maxVol = cap, prevState = None, filler = None))
  }

  val zeroState = Vector[Double](0.0)

  def mkGlsTup(g: Gls) = (g.lab -> g)

  def mk(lab: String, vol: Double, maxVol: Double, prevState: Option[Gls], filler: Option[Gls]): Gls = {
    new Gls(lab = lab, _vol = vol, maxVol = maxVol, filler = filler, prevState = prevState)
  }

}

object Tap extends Gls(lab = "Tap", _vol = 1e10, maxVol = 1e20, filler = None, prevState = None) {
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
class Gls(
  val lab: String,
  _vol: Double,
  val maxVol: Double,
  filler: Option[Gls],
  prevState: Option[Gls]) {
  /* Body */
  private val src = this
  val vol = if (_vol > maxVol) maxVol else _vol
  val avail: Double = maxVol - vol
  val isEmpty = vol == 0.0
  val isFull = vol == maxVol
  val isSemiFull = !isEmpty && !isFull
  val prevVols = extractVols toVector //_prevVols :+ vol

  def extractVols: List[Double] = this.prevState match {
    case None => List()
    case null => List()
    case _ => this.vol :: this.prevState.get.extractVols
  }

  override def toString =
    /*lab + "_" + */ "[" + vol + " / " + maxVol + "]" //+":" + math.round(vol / maxVol * 100) + "%"

  private def _afterPouring(dst: Gls): (Double, Double) = {
    val transportable = math.min(src.vol, dst.avail)
    (vol - transportable, dst.vol + transportable)
  }

  def FillFull = Gls.mk(lab = this.lab, vol = maxVol, maxVol = maxVol, filler = Some(Tap), prevState = Some(this))
  def Empty = Gls.mk(lab = lab, vol = 0.0, maxVol = maxVol, filler = None, prevState = Some(this))

  //def Clone(vol: Double, src: Gls)

  def PourTo(dst: Gls): Gls = {
    if (src.isEmpty) {
      dst
    } else if (dst.lab == src.lab) {
      dst
    } else {
      val (srcVol, dstVol) = _afterPouring(dst)
      val srcState = Gls.mk(lab = src.lab, vol = srcVol, maxVol = src.maxVol, filler = src.Filler, prevState = Some(src))
      val dstState = Gls.mk(lab = dst.lab, vol = dstVol, maxVol = dst.maxVol, filler = Some(srcState), prevState = Some(dst))
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
    case Some(Tap) => List(Tap)
    case _ => this :: this.Filler.get.History
  }
  
  def ListHistory: List[Gls] = _listHistory(Some(this))
  private def _listHistory(g: Option[Gls]): List[Gls] = {
    if (g.isEmpty)
      List[Gls]()
    else {
      val parents = (prevState, filler) match {
        case (None, None) => List[Gls]()
        case (None, f) => List[Gls](f.get)
        case (p, None) => List[Gls](p.get)
        case (p, f) => List[Gls](p.get, f.get)
      }
      parents
    }

  }

  def PrintHistory: List[List[Gls]] = {
    val prev = _listHistory(this.prevState)
    val filler = _listHistory(this.filler)
    val me = List(this)
    me :: prev :: List(filler)
  }

  import scala.Nothing
  def Filler: Option[Gls] = filler //.getOrElse(Tap)
  def WithFiller: Vector[Gls] = if (this.Filler.isEmpty) Vector(this) else Vector(this, this.Filler.get)
}

















