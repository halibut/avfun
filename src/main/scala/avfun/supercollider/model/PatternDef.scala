package avfun.supercollider.model




case class NoteEvent(time:Float, freq:Float, dur:Float, pan:Float)

case class PatternDef(notes:Seq[NoteEvent], length:Float) {
  def offsetBy(amt:Float):PatternDef = {
    copy(notes = notes.map(n => n.copy(time = n.time + amt) ) )
  }
  def append(o:PatternDef) = {
    PatternDef(this.notes ++ o.offsetBy(this.length).notes, this.length + o.length) 
  }
}
