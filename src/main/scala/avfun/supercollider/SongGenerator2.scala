package avfun.supercollider

import avfun.nnviz.ga.Organism
import avfun.nnviz.ga.Organism
import avfun.supercollider.SongStructureOrganism.SongStructureOrganismDef
import avfun.nnviz.nn.FastNetwork
import avfun.supercollider.InstrumentPatternOrganism.InstrumentPatternOrganismDef
import avfun.supercollider.PatternStructureOrganism.PatternStructureOrganismDef
import avfun.nnviz.ga.Organism
import scala.annotation.tailrec

object SongGenerator2 extends App {
  
  def randSongPart():PatternType = {
    val rndInd = (math.random * 500).toInt % 5
    
    val songPart = rndInd match {
        case 0 => Intro()
        case 1 => Outro()
        case 2 => Melody()
        case 3 => Chorus()
        case 4 => Break()
      }
    
    songPart
  }

  val interestingPatternStructureOrganisms = FastOrganismTrainer.train(PatternStructureOrganismDef, 30, 25){ o =>
    val notes = (0 until 20).map{ i =>
      val songPart = randSongPart();
      val pInput = PatternInput(songPart, (math.random * 16).toInt, (math.random * 16).toInt, (math.random * 16).toInt)
      
      val ps = PatternStructureOrganism.getPatternStructure(o, pInput)
      (ps.noteDiv, ps.noteOffsetRange)
    }
    
    notes.sliding(2).map(s => math.abs(s(1)._1 - s(0)._1) + math.abs(s(1)._2 - s(0)._2)).sum
  }
  
  val interestingPatternOrganisms = FastOrganismTrainer.train(InstrumentPatternOrganismDef, 30, 80){ o =>
    
    val notes = (0 until 20).map{ i =>
      val songPart = randSongPart();
      
      val pInput = PatternInput(songPart, (math.random * 16).toInt, (math.random * 16).toInt, (math.random * 16).toInt)
      val structure = PatternStructureOrganism.getPatternStructure(genPatternStructureOrganism, pInput).copy(noteOffsetRange = 16)
      
      val pattern = InstrumentPatternOrganism.getPattern(o, structure, pInput)
      
      val numNotesOn = pattern.notes.filter(_.isDefined).size
      
      val notesOn = numNotesOn.toFloat / pattern.notes.size.toFloat
      
      val sliding4NoteDiff = 
        pattern.notes.filter(_.isDefined).map(_.get._2).sliding(4).map{x => x.distinct.size.toFloat }.sum / 4f
        
      val arpDiff = 
        pattern.notes.filter(_.isDefined).map(_.get._2).sliding(2).map{x =>
          if(x.size == 2) {
            val diff = math.abs(x(1) - x(0))
            if(diff == 1 || diff == 2) {
              1f
            }
            else if(diff == 3 || diff == 4) {
              0.5f
            }
            else if(diff != 0) {
              0.25f
            }
            else {
              0f
            }
          }
          else {
            0f
          }
        }.sum
      
      
      (sliding4NoteDiff + arpDiff) / pattern.notes.size.toFloat
    }
    
    notes.sum
  }
  
  def genSongStructureOrganism():Organism[SongStructureOrganismDef.type] = {
    var org = SongStructureOrganismDef.randomize;
    val orgs = for(i <- 0 until 30) yield {
      org = SongStructureOrganismDef.mate(org, SongStructureOrganismDef.randomize);
    }
    org
  }
  
  def genPatternStructureOrganism():Organism[PatternStructureOrganismDef.type] = {
    interestingPatternStructureOrganisms(math.floor(math.random * interestingPatternStructureOrganisms.size / 2).toInt)
  }
  
  def genPatternOrganism():Organism[InstrumentPatternOrganismDef.type] = {
    val size = interestingPatternOrganisms.size / 2
//    val o1 = interestingPatternOrganisms(math.floor(math.random * size).toInt)
//    val o2 = interestingPatternOrganisms(math.floor(math.random * size).toInt)
//    InstrumentPatternOrganismDef.mate(o1, o2)
    interestingPatternOrganisms(math.floor(math.random * size).toInt)
  }
  
  def genPattern(
    instrumentInd: Int, bars:Int, repeat:Int, 
    patternType: PatternType, scale: MusicScale, rootNote:Int, songNoteDiv:Int,
    patternNet:FastNetwork,
    patternStructureOrg:Organism[PatternStructureOrganismDef.type]) : Seq[(String,Float)] = {
    
    //Create a pattern for each bar and then append them together
    val barPatterns = for(b <- 0 until bars) yield {
      val patternInput = PatternInput(patternType, b+1, instrumentInd, songNoteDiv)
      val patternStructure = PatternStructureOrganism.getPatternStructure(patternStructureOrg, patternInput);
      val pattern = InstrumentPatternOrganism.getPatternFromNet(patternNet, patternStructure, patternInput)
      val lenMult = patternStructure.noteLength
      
      val notePattern = pattern.notes.map{ n =>
        n match {
          case None => Seq(("\\rest", lenMult))
          case Some((l,o)) => {
            val negLen = 1f - l
            val realLen = l * lenMult;
            val remaining = negLen * lenMult;
            val noteOffset = scale.getOffset(rootNote, o)
            Seq((noteOffset.toString(), realLen), ("\\rest", remaining)) 
          }
        }
      }
      
      notePattern.flatten
    }
    
    val repeated = for(r <- 0 until repeat; p <- barPatterns.flatten) yield p 
    
    val uncondensed = repeated
    
    //Combine \\rests if they are next to each other
    //Remove useless rests and notes (that have duration of 0.0
    var condensed = Seq[(String,Float)](uncondensed.head)
    for((s,f) <- uncondensed.tail) {
      val last = condensed.last
      if(f <= 0.0f) {
        //don't do anything
      }
      //else if(last._1 == s && s == "\\rest") {
      else if(last._1 == s) {
        condensed = condensed.dropRight(1) :+ (s, last._2 + f)
      }
      else {
        condensed :+= (s, f)
      }
    }
    
    //Supercollider has a problem if the first note has zero duration
    while(condensed.head._2 <= 0.0f) {
      condensed = condensed.tail
    }
    
    condensed
  }
  
  def printInstrumentMelody(insInd:Int, instrumentPattern:FastNetwork,
      scale:MusicScale, rootNote:Int, patternOrg:Organism[PatternStructureOrganismDef.type],
      songStructure:SongStructure, synths:Int):Unit = {
    val ins = instrumentPattern
    val noteDiv = songStructure.noteDiv
    val intro = if(songStructure.hasIntro) Some(genPattern(insInd, 4, 2, Intro(), scale, rootNote, noteDiv, ins, patternOrg)) else None
    val outro = if(songStructure.hasOutro) Some(genPattern(insInd, 4, 2, Outro(), scale, rootNote, noteDiv, ins, patternOrg)) else None
    val chorus = if(songStructure.hasChorus) Some(genPattern(insInd, 8, 2, Chorus(), scale, rootNote, noteDiv, ins, patternOrg)) else None
    val break = if(songStructure.hasBreak) Some(genPattern(insInd, 16, 1, Break(), scale, rootNote, noteDiv, ins, patternOrg)) else None
    val melody1 = Some(genPattern(insInd, 8, 2, Melody(), scale, rootNote, noteDiv, ins, patternOrg))
    val melody2 = Some(genPattern(insInd, 8, 2, Melody(), scale, rootNote, noteDiv, ins, patternOrg))
    
//    val ins = instrumentPattern
//    val intro = if(songStructure.hasIntro) Some(genPattern(insInd, 16, 1, Intro(), scale, rootNote, ins, patternOrg)) else None
//    val outro = if(songStructure.hasOutro) Some(genPattern(insInd, 16, 1, Outro(), scale, rootNote, ins, patternOrg)) else None
//    val chorus = if(songStructure.hasChorus) Some(genPattern(insInd, 4, 4, Chorus(), scale, rootNote, ins, patternOrg)) else None
//    val break = if(songStructure.hasBreak) Some(genPattern(insInd, 8, 2, Break(), scale, rootNote, ins, patternOrg)) else None
//    val melody1 = Some(genPattern(insInd, 8, 2, Melody(), scale, rootNote, ins, patternOrg))
//    val melody2 = Some(genPattern(insInd, 8, 2, Melody(), scale, rootNote, ins, patternOrg))
    
    val fullSongForIns = Seq(
        ("intro",intro), 
        ("melody1", melody1), 
        ("chorus", chorus), 
        ("melody2", melody2), 
        ("break", break), 
        ("chorus", chorus), 
        ("outro", outro))
    
    val midi = for((name, notes) <- fullSongForIns) yield {
      notes match {
        case Some(seq) => {
          Seq("        //"+name, 
              "        "+seq.map(_._1).mkString(", "))
        }
        case None => Seq[String]()
      }
    } 
    
    val lengths = for((name, notes) <- fullSongForIns) yield {
      notes match {
        case Some(seq) => {
          Seq("        //"+name, 
              "        "+seq.map(_._2).mkString(", "))
        }
        case None => Seq[String]()
      }
    } 
    
    val printableInstInd = ((insInd%synths)+1)
    
    println("//Instrument - "+printableInstInd)
    println("i"+(insInd+1)+" = Pbind(")
    println("  \\instrument, \"inst"+printableInstInd+"\",")
    println("  \\midinote, Pseq([")
    println(midi.flatten.mkString(",\n"))
    println("        ], 1),")
    println("  \\dur, Pseq([")
    println(lengths.flatten.mkString(",\n"))
    println("        ], 1)")
    println(");")
  }
  
  @tailrec
  def getInterestingSongStructure():(Organism[SongStructureOrganismDef.type],SongStructure) = {
    var songOrg = genSongStructureOrganism()
    var songStructure = SongStructureOrganism.getSongStructure(songOrg)
    
    val parts = Seq(songStructure.hasBreak, songStructure.hasChorus, songStructure.hasIntro, songStructure.hasOutro)
      .filter(_ == true).size
    
    if(parts >= 3 && Seq(2, 3, 4, 5).exists(_ == songStructure.noteDiv) && songStructure.instruments >= 3) {
      (songOrg, songStructure)
    }
    else {
      getInterestingSongStructure()
    }
  }
  
  def genSong() {
    val (songOrg, songStructure) = getInterestingSongStructure()
    
    val synths = math.min(songStructure.instruments, 6)
    
    val instrumentPatterns = for(i <- 0 until math.min(12, songStructure.instruments)) yield {
      InstrumentPatternOrganismDef.expressPhenotype(genPatternOrganism())
    }
    
    val patternOrg = genPatternStructureOrganism()
    
    val scale = new MajorScale(4, MusicScale.G)
    val rootNote = scale.originNote
    
  
    println("//"+songStructure)
    
    val numInstruments = instrumentPatterns.size
    val varDeclaration = ((1 to numInstruments).toSeq.map("i"+_).mkString(", ")); 
    
    println("(");
    println("var "+varDeclaration+";");
    println();
    
    for(i <- 1 until (synths+1)) {
      val (org,synthDef) = SCSynthWriter.getGoodSynthFromSavedDefs()
      SCSynthWriter.saveSynthDef("n-"+i, org)
      println(SCSynthWriter.writeSynthDef(synthDef, "inst"+i, true)+".add;\n\n")
    }
    
    for(insInd <- 0 until numInstruments) {
      printInstrumentMelody(insInd, instrumentPatterns(insInd), scale, rootNote, patternOrg, songStructure, synths)
      println();
    }
    println();
    println("Ppar([ "+varDeclaration+" ]).play(TempoClock("+(songStructure.bpm/120f)+"));")
    println(")");

    
    
  }

  genSong();

}