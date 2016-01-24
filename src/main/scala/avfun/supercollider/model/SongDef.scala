package avfun.supercollider.model

case class SongDef(bpm:Float, noteDivs:Int, notes:Seq[(String,SynthDef,Seq[PatternDef])]) {
  val tempo = bpm / 60f
  val noteLengthMult = 1f / tempo 
}