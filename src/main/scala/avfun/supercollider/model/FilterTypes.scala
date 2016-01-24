package avfun.supercollider.model

trait FilterType {
  val typeInd:Int
  def getVars:UGenVars 
  val filterAmt:Float
}

case class HighPassFilter(cutoffFreq:Float, relative:Boolean = false, followEnv:Option[Float] = None, filterAmt:Float = 1f) extends FilterType {
  def this(vars:UGenVars, filterAmt:Float) = this(
      FilterType.getHighFreq(vars),
      FilterType.getRelative(vars),
      FilterType.getFolowEnvOpt(vars),
      filterAmt)
  override def getVars:UGenVars = UGenVars()
    .update(1, FilterType.getFreqVar(relative, cutoffFreq))
    .update(2, FilterType.getRelativeVar(relative))
    .update(3, FilterType.getFollowEnvOptVar(followEnv))
  override val typeInd = 0
}

case class LowPassFilter(cutoffFreq:Float, relative:Boolean = false, followEnv:Option[Float] = None, filterAmt:Float = 1f) extends FilterType {
  def this(vars:UGenVars, filterAmt:Float) = this(
      FilterType.getLowFreq(vars),
      FilterType.getRelative(vars),
      FilterType.getFolowEnvOpt(vars),
      filterAmt)
  override def getVars:UGenVars = UGenVars()
    .update(0, FilterType.getFreqVar(relative, cutoffFreq))
    .update(2, FilterType.getRelativeVar(relative))
    .update(3, FilterType.getFollowEnvOptVar(followEnv))
  override val typeInd = 1
}

case class BandPassFilter(lowCutoffFreq:Float, highCutoffFreq:Float, relative:Boolean = false, followEnv:Option[Float] = None,  filterAmt:Float = 1f) extends FilterType {
  def this(vars:UGenVars, filterAmt:Float) = this(
    if(vars(0) < vars(1)) FilterType.getLowFreq(vars) else FilterType.getLowFreq(vars.update(0, vars(1)).update(1, vars(0))), 
    if(vars(0) < vars(1)) FilterType.getHighFreq(vars) else FilterType.getHighFreq(vars.update(0, vars(1)).update(1, vars(0))),
    FilterType.getRelative(vars),
    FilterType.getFolowEnvOpt(vars),
    filterAmt)
  
  override def getVars:UGenVars = UGenVars()
    .update(0, FilterType.getFreqVar(relative, lowCutoffFreq))
    .update(1, FilterType.getFreqVar(relative, highCutoffFreq))
    .update(2, FilterType.getRelativeVar(relative))
    .update(3, FilterType.getFollowEnvOptVar(followEnv))
  override val typeInd = 2
}

case class FormletFilter(resFreqRatio:Float, attackTime:Float, decayTime:Float, filterAmt:Float = 1f) extends FilterType {
  def this(vars:UGenVars, filterAmt:Float) = this(
      vars(0) * 10f - 5f,
      vars(2) * 2f,
      vars(3) * 2f,
      filterAmt)
  override def getVars:UGenVars = UGenVars()
    .update(0, (resFreqRatio+5f) / 10f)
    .update(2, attackTime / 2f)
    .update(3, decayTime / 2f)
  override val typeInd = 3
}

case class MoogFilter(cutoffFreq:Float, resAmt:Float = 2f, followEnv:Option[Float] = None, filterAmt:Float = 1f) extends FilterType {
  def this(vars:UGenVars, filterAmt:Float) = this(
      FilterType.getFreq(true, vars(0)),
      vars(3) * 4f,
      FilterType.getFolowEnvOpt(vars),
      filterAmt)
  override def getVars:UGenVars = UGenVars()
    .update(0, FilterType.getFreqVar(true, cutoffFreq))
    .update(2, resAmt / 4f)
    .update(3, FilterType.getFollowEnvOptVar(followEnv))
  override val typeInd = 4
}

case object InvalidFilter extends FilterType {
  override def getVars:UGenVars = UGenVars()
  override val typeInd = -1
  override val filterAmt = 0f
}

object FilterType {
  
  def getType(ind:Int, vars:UGenVars, filterAmt:Float):FilterType = ind match {
    case 0 => new HighPassFilter(vars, filterAmt)
    case 1 => new LowPassFilter(vars, filterAmt)
    case 2 => new BandPassFilter(vars, filterAmt)
    case 3 => new FormletFilter(vars, filterAmt)
    case 4 => new MoogFilter(vars, filterAmt)
    case _ => InvalidFilter
  }
  
  
  def getHighFreq(vars:UGenVars):Float = {
    getFreq(vars(2) > 0.5f, vars(1))
  }
  def getLowFreq(vars:UGenVars):Float = {
    getFreq(vars(2) > 0.5f, vars(0))
  }
  def getFreq(relative:Boolean, floatVal:Float):Float = {
    20f + (if(relative) 5000f else 20000f) * floatVal
  }
  def getFreqVar(relative:Boolean, freq:Float):Float = {
		(freq-20f) / (if(relative) 5000f else 20000f)
  }
  
  def getRelative(vars:UGenVars):Boolean = {
    vars(2) > 0.5f
  }
  def getRelativeVar(relative:Boolean):Float = {
    if(relative) 0.75f else 0.25f
  }
  
  private val followMult = 4f
  
  def getFolowEnvOpt(vars:UGenVars):Option[Float] = {
    if(vars(3) < 0.25f) 
      Some( (vars(3)-0.25f)*followMult) 
    else if (vars(3) > 0.75f) 
      Some((vars(3)-.75f)*followMult) 
    else 
      None
  }
  def getFollowEnvOptVar(follow:Option[Float]):Float = {
    follow match {
      case Some(f) => {
        if(f < 0f) 
          .25f - (f / followMult)
        else
          .75f + (f / followMult)
      }
      case None => 0.5f
    }
  }
}

