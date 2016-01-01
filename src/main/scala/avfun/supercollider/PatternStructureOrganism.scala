package avfun.supercollider

import avfun.nnviz.ga.OrganismDef
import avfun.nnviz.ga.ChromosomeDef
import avfun.nnviz.ga.NumericChromosomeDef
import avfun.nnviz.ga.NumericExtImplicits._
import avfun.nnviz.nnga.FastNetworkOrganismDefinition
import avfun.nnviz.nn.ActivationFunc
import avfun.nnviz.nn.LinearSaturationActivation
import avfun.nnviz.ga.Organism

object PatternStructureOrganism {
  
  //Inputs
  val songParts = Seq("isIntro", "isOutro", "isChorus", "isMelody", "isBreak")
  val barBits = Seq("bar_b1", "bar_b2", "bar_b3", "bar_b4", "bar_b5", "bar_b6", "bar_b7", "bar_b8")
  val insBits = Seq("ins_b1", "ins_b2", "ins_b3", "ins_b4", "ins_b5", "ins_b6", "ins_b7", "ins_b8")
  val songNoteDivBits = Seq("sNoteDiv_b1", "sNoteDiv_b2", "sNoteDiv_b3", "sNoteDiv_b4")
  
  val songPartsOffset = 0
  val barBitsOffset = songPartsOffset + songParts.size
  val insBitsOffset = barBitsOffset + barBits.size
  val songNoteDivBitsInd = insBitsOffset + insBits.size
  
  val inputNames = songParts ++ barBits ++ insBits ++ songNoteDivBits
  
  
  //Outputs
  val noteDivFrom = Seq("noteDiv_song", "noteDiv_pat_free", "noteDiv_pat_mult", "noteDiv_pat_double", "noteDiv_pat_triple", "noteDiv_pat_half")
  val noteDivBits = Seq("noteDiv_b1", "noteDiv_b2", "noteDiv_b3", "noteDiv_b4")
  val noteOffsetRangeBits = Seq("noteOffsetRange_b1", "noteOffsetRange_b2", "noteOffsetRange_b3", "noteOffsetRange_b4", "noteOffsetRange_b5")
  
  val noteDivFromInd = 0
  val noteDivBitsOffset = noteDivFromInd + noteDivFrom.size
  val noteOffsetRangeBitsOffset = noteDivBitsOffset + noteDivBits.size
  val fullNoteInd = noteOffsetRangeBitsOffset + noteOffsetRangeBits.size
  
  val outputNames = noteDivFrom ++ noteDivBits ++ noteOffsetRangeBits ++ Seq("fullNote")
  
  val numInputs = inputNames.size
  val numOutputs = outputNames.size
  val numHiddenNeurons = 2 * (numInputs + numOutputs) 
  val numConnections = 10 * (numInputs + numOutputs + numHiddenNeurons)
  
  object PatternStructureOrganismDef extends FastNetworkOrganismDefinition(
    name = "Pattern Structure Organism", 
    numInputs = PatternStructureOrganism.numInputs,
    numOutputs = PatternStructureOrganism.numOutputs,
    numHiddenNeurons = PatternStructureOrganism.numHiddenNeurons,
    numConnections = PatternStructureOrganism.numConnections,
    activationFunc = LinearSaturationActivation) {
    
    implicit val networkConv = FastNetworkOrganismDefinition.implicitlyGetFastNetworkConverter(PatternStructureOrganismDef)
    
  }

  def getPatternStructure(org: Organism[PatternStructureOrganismDef.type], input:PatternInput): PatternStructure = {
    val net = org.organismDefintion.expressPhenotype(org)(PatternStructureOrganismDef.networkConv)
    net.preRun(10)(1.0f)
    
    val inputs = new Array[Float](inputNames.size);
    
    for(i <- 0 until songNoteDivBits.size) {
      inputs(songNoteDivBitsInd + i) = MathUtil.intToBits(input.songNoteDiv)(i)
    }
    
    for(i <- 0 until insBits.size) {
      inputs(insBitsOffset + i) = MathUtil.intToBits(input.instrumentInd)(i)
    }
    
    for(i <- 0 until barBits.size) {
      inputs(barBitsOffset + i) = MathUtil.intToBits(input.bar)(i)
    }
    
    input.patType match {
      case Intro() => inputs(songPartsOffset+0) = 1f
      case Outro() => inputs(songPartsOffset+1) = 1f
      case Chorus() => inputs(songPartsOffset+2) = 1f
      case Melody() => inputs(songPartsOffset+3) = 1f
      case Break() => inputs(songPartsOffset+4) = 1f
    }
    val outputs = net.calc(inputs)
    
    
    
    val noteDivFromSong = outputs(noteDivFromInd)
    val noteDivFromPatternFree = outputs(noteDivFromInd+1)
    val noteDivFromPatternMult = outputs(noteDivFromInd+2)
    
    val noteDiv = if(noteDivFromSong >= noteDivFromPatternFree && noteDivFromSong >= noteDivFromPatternMult) {
      input.songNoteDiv
    }
    else if(noteDivFromPatternMult >= noteDivFromPatternFree) {
      input.songNoteDiv * 
        (if(outputs(noteDivFromInd+3)>=0f) 2 else 1) * 
        (if(outputs(noteDivFromInd+4)>=0f) 3 else 1) /
        (if(outputs(noteDivFromInd+5)>=0f) 2 else 1)
    }
    else {
      val noteDivSlice = outputs.slice(noteDivBitsOffset, noteDivBitsOffset + noteDivBits.size)
      1 + MathUtil.bitsToInt(noteDivSlice)
    }
    
    val noteOffsetBitsSlice = outputs.slice(noteOffsetRangeBitsOffset, noteOffsetRangeBitsOffset + noteOffsetRangeBits.size)
    val noteOffset = 3 + MathUtil.bitsToInt(noteOffsetBitsSlice) % 24
    
    val realNoteDiv = math.max(1, (noteDiv % 16))
    
    val fullNote = outputs(fullNoteInd) >= 0.5f
    
    PatternStructure(realNoteDiv, noteOffset, fullNote)
  }
  
  
}


