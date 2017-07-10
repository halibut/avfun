package avfun.supercollider.model

import avfun.supercollider.MathUtil._
import avfun.supercollider.MathUtil

case class UGenVars(values:Seq[Float]) {
  def this() = this(new Array[Float](4))
  
  require(values.size == 4)
  
  def apply(i:Int):Float = values(i)
  def update(i:Int, f:Float): UGenVars = UGenVars(values.updated(i, clamp_0_1(f)))
  def merge(index:Int, other:UGenVars):UGenVars = UGenVars(this.values.slice(0, index) ++ other.values.slice(index, 8))
}

object UGenVars {
  def apply() = new UGenVars()
}

sealed trait UGenFreqControl{
  def prettyPrint():String
}
case class StaticFreq(freq:Float) extends UGenFreqControl {
  def prettyPrint():String = s"StaticFreq(freq = ${freq})"
}
case class DynamicFreq(freqMult:Float = 1f, detuneAmt:Float = 0f) extends UGenFreqControl {
  def totalFreqMult = freqMult + detuneAmt
  def prettyPrint():String = s"DynamicFreq(freqMult = ${freqMult}, detuneAmt = ${detuneAmt})"
}


case class LFODef(speed:Float, amount:Float)
case class UGenDef(uGenType:UGenType, amp:Float, freqControl:UGenFreqControl, envelopeType:EnvelopeType, filterType:Option[FilterType]) {
  def isValid = uGenType != InvalidUGen && envelopeType != InvalidEnvelope && !filterType.exists(f => f == InvalidFilter)
  
  def prettyPrint():String = {
    s"""Generator(
    |  Type: ${uGenType.getClass.getSimpleName} [${uGenType.typeId}]
    |  Amplitude: ${amp}
    |  Frequency Control: ${freqControl.prettyPrint()}
    |  Envelope: ${envelopeType.prettyPrint()}
    |)""".stripMargin
  }
}
case class SynthDef(lfoAmp:Option[LFODef], lfoFreq:Option[LFODef], ugens:Seq[Option[UGenDef]], vars:UGenVars) {
  def longestUGen:Option[UGenDef] = {
    
    val actualUgens = ugens.filter(_.isDefined).map(_.get)
    if(actualUgens.isEmpty) {
      None
    }
    else {
      val longest = actualUgens.sortBy(_.envelopeType.minTime).last
      Some(longest)
    }
  }
  def isValid = {
    val ugenCol = ugens.filter(_.isDefined).map(_.get)
    ugenCol.size > 0 && ugenCol.forall(_.isValid)
  }
  
  def prettyPrint():String = {
    s"""Synth(
    |  LFO Amplitude: ${lfoAmp.map(lfo => lfo.speed +"hz - "+lfo.amount+" range").getOrElse("N/A")}
    |  LFO Frequency: ${lfoAmp.map(lfo => lfo.speed +"hz - "+lfo.amount+" range").getOrElse("N/A")}
    |  Generators:
    |    ${ugens.map(_.map(_.prettyPrint().split("\n").mkString("\n    ")).getOrElse("N/A")).mkString("\n    ")}
    |  Variable Values: ${vars.values.mkString(",")}
    |)""".stripMargin
  }
}