package avfun.supercollider.model

trait SongPartRepeatType {
  def doRepetition(pDefs:Seq[PatternDef]):PatternDef
}

object RepeatAllOnceRepeatType extends SongPartRepeatType {
  
  override def doRepetition(pDefs:Seq[PatternDef]):PatternDef = {
    val once = pDefs.reduceLeft((p1,p2) => p1 append p2)
    once append once
  }
}

object RepeatEachOnceRepeatType extends SongPartRepeatType {
  override def doRepetition(pDefs:Seq[PatternDef]):PatternDef = {
    val doubled = pDefs.map(p => p append p)
    doubled.reduceLeft((p1,p2) => p1 append p2)
  }
}


case class SongPartDef(index:Int, bars:Int) 

object SongPart {
  
  val intro = Seq(0)
  val melody = Seq(1, 3)
  val chorus = Seq(2, 5)
  val break = Seq(4)
  val outro = Seq(6)
  
  def getSongPart(partIndex:Int, totalParts:Int):Seq[SongPartDef] = {
    val divTimes = partIndex / 5
    val remainder = partIndex % 5
    
    remainder match {
      case 0 => melody.map(i => SongPartDef(i * totalParts + divTimes, 4))
      case 1 => chorus.map(i => SongPartDef(i * totalParts + divTimes, 4))
      case 2 => break.map(i => SongPartDef(i * totalParts + divTimes, 4))
      case 3 => intro.map(i => SongPartDef(i * totalParts + divTimes, 4))
      case 4 => outro.map(i => SongPartDef(i * totalParts + divTimes, 4))
    }
  }
}