package avfun.supercollider.model

sealed trait EnvelopeType {
  val uGenVals:UGenVars
  def hasSustain:Boolean
  def minTime:Float
  val typeInd:Int
}
case class EnvelopeADSR(attackTime:Float, decayTime:Float, sustainLevel:Float, releaseTime:Float) extends EnvelopeType {
  def this(vars:UGenVars) = this(
      0.001f + vars(0),
      0.01f + vars(3),
      0.25f + 0.75f * vars(2),
      0.01f + 2f * vars(1)
    )
  override val hasSustain = true
  override val typeInd = 0
  override val uGenVals = UGenVars()
    .update(0, attackTime-0.001f)
    .update(3, decayTime-0.01f)
    .update(2, (sustainLevel-0.25f) / 0.75f)
    .update(1, (releaseTime - 0.01f) / 2f)

  override val minTime:Float = attackTime + releaseTime + decayTime
}
case class EnvelopePerc(val attackTime:Float, val releaseTime:Float) extends EnvelopeType {
  def this(vars:UGenVars) = this(
      0.001f + 0.25f * vars(0),
      0.01f + 2f * vars(1)
    )
  override val hasSustain = false
  override val typeInd = 0
  
  override val uGenVals = UGenVars()
    .update(0, (attackTime-0.001f) / 0.25f)
    .update(1, (releaseTime - 0.01f) / 2f)
  
  override def minTime = attackTime + releaseTime
}

case object InvalidEnvelope extends EnvelopeType {
  override val uGenVals:UGenVars = UGenVars()
  override def hasSustain:Boolean = false
  override def minTime:Float = 0f
  override val typeInd:Int = -1
}


object EnvelopeType {
  def getType(ind:Int, vars:UGenVars):EnvelopeType = ind match {
    case 0 => new EnvelopeADSR(vars)
    case 1 => new EnvelopePerc(vars)
    case _ => InvalidEnvelope
  }
}