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
    bpm:Float, instruments:Int, noteDiv:Int, scaleOffsetFromA4:Int)

object MathUtil {
  def intToBits(intNum:Int, zero:Float = 0f, one:Float = 1f)(bitNum:Int): Float = {
    if(bitNum == 0) {
      if(intNum % 2 == 1) one else zero
    }
    else {
      intToBits(intNum / 2, zero, one)(bitNum -1)
    }
  }
  
  def bitsToInt(bits: Seq[Float], gate:Float = 0.0f): Int = {
    val ints = bits.zipWithIndex.map{case(f,i) =>
      if(f > gate) {
        math.pow(2, i).toInt
      }
      else {
        0
      }
    }
    ints.sum
  }
  
  def clamp(min:Float, value:Float, max:Float):Float = {
    math.max(min, math.min(value, max))
  }
  
  def clamp_0_1(value:Float):Float = {
    clamp(0f, value, 1f)
  }
}