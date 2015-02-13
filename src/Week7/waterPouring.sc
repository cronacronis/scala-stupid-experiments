package Week7
object waterPouring {
  val A = Gls(lab = "A", _vol = 4, maxVol = 5, filler = null)
                                                  //> A  : Week7.Gls = A_[4.0 / 5.0]
  val b = Gls(lab = "b", _vol = 2, maxVol = 3, filler = null)
                                                  //> b  : Week7.Gls = b_[2.0 / 3.0]
  //A pourTo b pourTo A pourTo b
  
  
  val dstAmount = 6                               //> dstAmount  : Int = 6
  val caps: Vector[Double] = Vector(4, 9)         //> caps  : Vector[Double] = Vector(4.0, 9.0)
  val glasses = caps.map(Gls.mkLb).toMap          //> glasses  : scala.collection.immutable.Map[String,Week7.Gls] = Map(4 -> 4_[0.
                                                  //| 0 / 4.0], 9 -> 9_[0.0 / 9.0])
  val totalCap = { for (glass <- glasses.values) yield glass.maxVol }.sum
                                                  //> totalCap  : Double = 13.0

  def repour(gs: Iterable[Gls]): Set[Gls] =
  { (	for
	  	{	src <- gs
	    	dst <- gs
	      if src.lab != dst.lab
	      val filler = if(src.isEmpty) src.fillFull else src
	    } yield (filler pourTo dst)
    ) toSet
	}                                         //> repour: (gs: Iterable[Week7.Gls])Set[Week7.Gls]
  repour(glasses.values)                          //> res0: Set[Week7.Gls] = Set(9_[4.0 / 9.0], 4_[4.0 / 4.0])
  repour(glasses.values).filter(p => p.isSemiFull).flatMap(g => List(g.filler.getOrElse(Tap), g))
                                                  //> res1: scala.collection.immutable.Set[Week7.Gls] = Set(4_[0.0 / 4.0], 9_[4.0 
                                                  //| / 9.0])
  
  def history(g: Gls): List[Gls] = g.filler match {
  	case null => List(g)
  	//case Some(Tap) => List(Tap)
  	case _ => g :: history(g.filler.get)
  }                                               //> history: (g: Week7.Gls)List[Week7.Gls]
  
  history(repour(glasses.values).head)            //> res2: List[Week7.Gls] = List(9_[4.0 / 9.0], 4_[0.0 / 4.0], Tap_[+/+])
  
  repour(repour(glasses.values)).filter(p => p.isSemiFull).flatMap(g => List(g.filler.getOrElse(Tap), g))
                                                  //> res3: scala.collection.immutable.Set[Week7.Gls] = Set(4_[0.0 / 4.0], 9_[8.0
                                                  //|  / 9.0])
  
}

object Gls {
  def mkLb(cap: Double): (String, Gls) = {
    val lab = cap.toInt.toString
    (lab -> Gls(lab, 0, cap, null))
  }
}

object Tap extends Gls("Tap", 1e10, 1e20, null){
	override def fillFull = Tap
	override def empty = Tap
	override def pourTo(dst: Gls) = Tap
	override def toString = lab + "_[+/+]"
}

case class Gls(
	val lab: String,
	_vol: Double,
	val maxVol: Double,
	val filler: Option[Gls]) {
  private val src = this
  val vol = if (_vol > maxVol) { spilled; maxVol } else _vol
  val avail: Double = maxVol - vol
	
	
	
  val isEmpty = vol == 0.0
  val isFull = vol == maxVol
  val isSemiFull = !isEmpty && !isFull
  def hasExact(amount: Double) = vol == amount

  private def fillWith(amount: Double) = Gls(this.lab, this.vol + amount, this.maxVol, Some(Tap))
  def fillFull = fillWith(maxVol)

  def empty = Gls(lab, 0.0, maxVol, null)

  def spilled = println("Spilled")
  
  override def toString =
    lab + "_" + "[" + vol + " / " + maxVol + "]" //+":" + math.round(vol / maxVol * 100) + "%"

  private def afterPouring(dst: Gls): (Double, Double) = {
    val transportable = math.min(src.vol, dst.avail)
    (vol - transportable, dst.vol + transportable)
  }

  def pourTo(dst: Gls): Gls = {
    if (src.isEmpty) dst
    else {
      val (srcVol, dstVol) = afterPouring(dst)
      val srcState = Gls(this.lab, srcVol, this.maxVol, this.filler)
      val dstState = Gls(dst.lab, dstVol, dst.maxVol, Some(srcState))
      dstState
    }
  }

  /*def pourtToEmpty(dst: Glass): (Glass, Glass) = {
    val (srcGlass, dstGlass) = pourTo(dst)
    ({ spilled; srcGlass.empty }, dstGlass)
  }*/
}