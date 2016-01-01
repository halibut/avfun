package avfun.supercollider


case class PatternStructure(noteDiv:Int, noteOffsetRange:Int, fullNote:Boolean) {
  val noteLength = 1.0f / noteDiv.toFloat
}

sealed trait PatternType
case class Intro() extends PatternType
case class Chorus() extends PatternType
case class Melody() extends PatternType
case class Break() extends PatternType
case class Outro() extends PatternType

case class PatternInput(patType:PatternType, bar:Int, instrumentInd:Int, songNoteDiv:Int) 

case class Pattern(notes:Seq[Option[(Float, Int)]])

case class SongStructure(
    hasIntro:Boolean, hasOutro:Boolean, hasChorus:Boolean, hasBreak:Boolean,
    bpm:Float, instruments:Int, noteDiv:Int)

sealed trait EnvelopeType
case class EnvelopeASDR() extends EnvelopeType
case class EnvelopePerc() extends EnvelopeType

case class LFODef(speed:Float, amount:Float)
case class UGenDef(uGenType:Int, amp:Float, freqMult:Float, envelopeType:EnvelopeType, filterType:Option[Int], vars:Seq[Float])
case class SynthDef(lfoAmp:Option[LFODef], lfoFreq:Option[LFODef], ugens:Seq[Option[UGenDef]], vars:Seq[Float])
    
object MathUtil {
  def intToBits(intNum:Int)(bitNum:Int): Float = {
    if(bitNum == 0) {
      intNum % 2
    }
    else {
      intToBits(intNum / 2)(bitNum -1)
    }
  }
  
  def bitsToInt(bits: Seq[Float]): Int = {
    val ints = bits.zipWithIndex.map{case(f,i) =>
      if(f > 0.0f) {
        math.pow(2, i).toInt
      }
      else {
        0
      }
    }
    ints.sum
  }
}