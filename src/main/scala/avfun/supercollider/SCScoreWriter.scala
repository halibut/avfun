package avfun.supercollider

import avfun.supercollider.model.SongDef
import avfun.supercollider.model.NoteEvent

object SCScoreWriter {
  
  def writeScore(songDef:SongDef):String = {
    
    val s = new StringBuilder();
    
    s.append("[\n")
    
    val notesAndTimes = for {
      (name, sDef, patterns) <- songDef.notes;
      p <- patterns;
      n <- p.notes
    } yield {
      (n, name)
    }
    
    val timesAndSynthDefs = for {
      ((n, name), ind) <- notesAndTimes.zipWithIndex
    } yield {
      Seq(
        (n.time * songDef.noteLengthMult, getNewSynthString(n, name, 1000 + ind)),
        (n.time * songDef.noteLengthMult + math.max(0.01f, n.dur), s"[\\n_set, ${1000 + ind}, \\gate, 0]")
      )
    }
    
    val sortedNotes = timesAndSynthDefs.flatten.sortBy(_._1)
   
    for((time, command) <- sortedNotes) {
      s.append("  [").append(time).append(", ").append(command).append("],\n");
    }
    
    val lastNoteTime = sortedNotes.last._1
    s.append("  [").append(lastNoteTime+3).append(", [\\c_set, 0, 0]]\n")
    
    s.append("\n]")
    s.toString()
  }
  
  private def getNewSynthString(e:NoteEvent, instName:String, nodeNumber:Int):String = {
    s"[\\s_new, \\${instName}, ${nodeNumber}, 0, 0, \\freq, ${e.freq}, \\gate, 1, \\pan, ${e.pan}]"
  }
  
}