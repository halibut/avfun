package avfun.supercollider

abstract class MusicScale {
  
  val midiIndices:Seq[Int] 
  
  val originNote:Int
  
  def getOffset(midiVal:Int, offsetNotes:Int):Int = {
    val ind = midiIndices.indexOf(midiVal)
    if(ind + offsetNotes >= midiIndices.length) {
      midiIndices.last
    }
    else if(ind + offsetNotes < 0) {
      midiIndices.head
    }
    else {
      midiIndices(offsetNotes + ind)
    }
  }
}

object MusicScale {
  val C = 0
  val C_# = 1
  val D = 2
  val D_# = 3
  val E = 4
  val F = 5
  val F_# = 6
  val G = 7
  val G_# = 8
  val A = 9
  val A_# = 10
  val B = 11
  
  def midiToFreq(midiIndex:Int):Float = {
    val exp = (midiIndex - 69).toDouble / 12
    math.pow(2, exp).toFloat * 440f
  }
  
  def freqToMidi(frequency:Float):Int = {
    val rounded = (math.log(frequency/440f)/math.log(2) * 12 + 69).round.toInt
    math.min(127, math.max(0, rounded))
  }
  
  trait OctaveIndicatorType
  object OctaveStartIndicator extends OctaveIndicatorType
  object OctaveAllNotesIndicator extends OctaveIndicatorType
  object OctaveNoIndicator extends OctaveIndicatorType
  
  def getMidiNoteStr(midiNote:Int, octaveIndicator:OctaveIndicatorType):String = {
    val octave = midiNote / 12
    val note = midiNote % 12 match {
      case 0 => "C" + (if(octaveIndicator == OctaveStartIndicator) "["+octave.toString+"]" else "")  
      case 1 => "C#"
      case 2 => "D"
      case 3 => "D#"
      case 4 => "E"
      case 5 => "F"
      case 6 => "F#"
      case 7 => "G"
      case 8 => "G#"
      case 9 => "A"
      case 10 => "A#"
      case 11 => "B"
    }
    
    if(octaveIndicator == OctaveAllNotesIndicator) {
      note+"_"+octave
    }
    else {
      note
    }
  }
}

abstract class AbstractScale(octave:Int, note:Int, indexOffsets:Seq[Int]) extends MusicScale {
  override val originNote = 12 * octave + note
  
  val scaleOffsets = indexOffsets.map(_ + note)
  
  override val midiIndices = for {
    o <- (-1 until 12)
    i <- scaleOffsets
    num = (12*o)+i 
    if(0 <= num && num < 128)
  } yield {
    num
  }
}

class MajorScale(octave:Int, note:Int) 
  extends AbstractScale(octave, note, Seq(0, 2, 4, 5, 7, 9, 11))


object CMajorScale extends MusicScale {
  
  import MusicScale._
  
  val originNote = 12 * 4 + C
  
  val midiIndices = for {
    o <- 0 until 11
    i <- Seq(C, D, E, F, G, A, B) //0,2,4,5,7,9,11
                                  //2,4,6,7,9,11,13
    num = (12*o)+i 
    if(0 <= num && num < 128)
  } yield {
    num
  }
  
}

object DMajorScale extends MusicScale {
  
  import MusicScale._
  
  val originNote = 12 * 4 + D
  
  val midiIndices = for {
    o <- 0 until 11
    i <- Seq(D, E, F_#, G, A, B, C_#)
    num = (12*o)+i 
    if(0 <= num && num < 128)
  } yield {
    num
  }
  
}