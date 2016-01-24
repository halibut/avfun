package avfun.supercollider.model

import avfun.supercollider.MathUtil

sealed trait UGenType{
  val typeId:Int
  def getUGenVars:UGenVars
}

case class UGenSin(phase:Float = 0f) extends UGenType {
  def this(vars:UGenVars) = this(vars(0))
  override val getUGenVars:UGenVars = UGenVars().update(0, phase)
  val typeId:Int = 0
}

case class UGenPulse(phase:Float = 0f) extends UGenType {
  def this(vars: UGenVars) = this(vars(0)) 
  override val getUGenVars:UGenVars = UGenVars().update(0, phase)
  val typeId:Int = 1
}

case class UGenSaw() extends UGenType {
  def this(vars: UGenVars) = this()
  override val getUGenVars:UGenVars = UGenVars()
	val typeId:Int = 2
}

case class UGenLFTri(phase:Float = 0f) extends UGenType {
  def this(vars: UGenVars) = this(vars(0))
  override val getUGenVars:UGenVars = UGenVars().update(0, phase)
  val typeId:Int = 3
}

case class UGenVarSaw(phase:Float = 0f, width:Float = .5f) extends UGenType {
  def this(vars:UGenVars) = this(vars(0), vars(1))
  override val getUGenVars:UGenVars = UGenVars().update(0, phase).update(1, width)
  val typeId:Int = 4
}

case class UGenBlip(start:Float = 5f, end:Float = 100f, time:Float = 1f)  extends UGenType {
  def this(vars: UGenVars) = this(5f + vars(0) * 100f, 5f + vars(1) * 100f, 0.01f + vars(2) * 2f)
  override val getUGenVars:UGenVars = UGenVars()
    .update(0, (start -5f)/100f )
    .update(1, (end -5f)/100f)
    .update(2, (time - 0.01f)/2f)
  val typeId:Int = 5
}

case class UGenFormant(formFreqMult:Int = 2, bwFreqMult:Int = 2) extends UGenType {
  def this(vars: UGenVars) = this(
      1 + MathUtil.bitsToInt(vars.values.slice(0,2), 0.5f), 
      1 + MathUtil.bitsToInt(vars.values.slice(0,4), 0.5f))
  override val getUGenVars:UGenVars = {
    UGenVars().update(0, MathUtil.intToBits(formFreqMult-1)(0))
      .update(1, MathUtil.intToBits(formFreqMult-1)(1))
      .update(2, MathUtil.intToBits(bwFreqMult-1)(2))
      .update(3, MathUtil.intToBits(bwFreqMult-1)(3))
  }
  val typeId:Int = 6
}

case class UGenWhiteNoise() extends UGenType {
  def this(vars: UGenVars) = this()
  override val getUGenVars:UGenVars = UGenVars()
  val typeId:Int = 7
}

case object InvalidUGen extends UGenType {
  override val getUGenVars:UGenVars = UGenVars()
  override val typeId = -1
}

object UGenType {
  def getType(ind:Int, vars:UGenVars):UGenType = ind match {
    case 0 => new UGenSin(vars)
    case 1 => new UGenPulse(vars)
    case 2 => new UGenSaw(vars)
    case 3 => new UGenLFTri(vars)
    case 4 => new UGenVarSaw(vars)
    case 5 => new UGenBlip(vars)
    case 6 => new UGenFormant(vars)
    case 7 => new UGenWhiteNoise(vars)
    
    case _ => InvalidUGen
  }
}