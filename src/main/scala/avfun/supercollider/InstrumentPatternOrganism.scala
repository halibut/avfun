package avfun.supercollider

import avfun.nnviz.ga.OrganismDef
import avfun.nnviz.ga.ChromosomeDef
import avfun.nnviz.ga.NumericChromosomeDef
import avfun.nnviz.ga.NumericExtImplicits._
import avfun.nnviz.nnga.FastNetworkOrganismDefinition
import avfun.nnviz.nn.ActivationFunc
import avfun.nnviz.nn.LinearSaturationActivation
import avfun.nnviz.ga.Organism
import avfun.nnviz.nn.FastNetwork


object InstrumentPatternOrganism {
  
  val songParts = Seq("isIntro", "isOutro", "isChorus", "isMelody", "isBreak")
  val barBits = Seq("bar_b1", "bar_b2", "bar_b3", "bar_b4", "bar_b5", "bar_b6", "bar_b7", "bar_b8")
  val noteDivBits = Seq("noteDiv_b1", "noteDiv_b2", "noteDiv_b3", "noteDiv_b4")
  val lastNoteOffsetBits = Seq("lastNoteOffset_b1", "lastNoteOffset_b2", "lastNoteOffset_b3", "lastNoteOffset_b4", "lastNoteOffset_b5") 
  
  val inputNames = songParts ++ barBits ++ noteDivBits ++ lastNoteOffsetBits
    Seq("lastOffsetSign", "lastNoteOn", "lastNoteLength");
  
  val songPartsOffset = 0
  val barBitsOffset = songPartsOffset + songParts.size
  val noteDivBitsOffset = barBitsOffset + barBits.size
  val lastNoteOffsetBitsOffset = noteDivBitsOffset + noteDivBits.size
  val offsetSignOffset = lastNoteOffsetBitsOffset + lastNoteOffsetBits.size + 1
  val lastNoteOnInd = offsetSignOffset + 1
  val lastNoteLengthInd = lastNoteOnInd + 1
  
  val noteOffsetBits = Seq("noteOffset_b1", "noteOffset_b2", "noteOffset_b3", "noteOffset_b4", "noteOffset_b5")
  val outputNames = noteOffsetBits ++ Seq("offsetSign", "noteOn", "noteLength");
  
  val numInputs = inputNames.size
  val numOutputs = outputNames.size
  
  val numHiddenNeurons = 2 * (numInputs + numOutputs) 
  val numConnections = 10 * (numInputs + numOutputs + numHiddenNeurons)
  
  object InstrumentPatternOrganismDef extends FastNetworkOrganismDefinition(
    name = "Instrument Pattern Organism", 
    numInputs = InstrumentPatternOrganism.numInputs,
    numOutputs = InstrumentPatternOrganism.numOutputs,
    numHiddenNeurons = InstrumentPatternOrganism.numHiddenNeurons,
    numConnections = InstrumentPatternOrganism.numConnections,
    activationFunc = LinearSaturationActivation) {
    
    implicit val networkConv = FastNetworkOrganismDefinition.implicitlyGetFastNetworkConverter(InstrumentPatternOrganismDef)
  }
  
  def getPatternFromNet(network:FastNetwork, structure:PatternStructure, patternInput:PatternInput):Pattern = {
    val inputs = new Array[Float](numInputs)
    patternInput.patType match {
      case Intro() => inputs(songPartsOffset+0) = 1f
      case Outro() => inputs(songPartsOffset+1) = 1f
      case Chorus() => inputs(songPartsOffset+2) = 1f
      case Melody() => inputs(songPartsOffset+3) = 1f
      case Break() => inputs(songPartsOffset+4) = 1f
    }
    
    for(i <- 0 until barBits.size) {
      inputs(barBitsOffset + i) = MathUtil.intToBits(patternInput.bar)(i) 
    }
    
    for(i <- 0 until noteDivBits.size) {
      inputs(noteDivBitsOffset + i) = MathUtil.intToBits(structure.noteDiv)(i) 
    }
    
    val pattern = for(i <- 0 until structure.noteDiv) yield {
      
      val outputs = network.calc(inputs);
      
      val offsets = outputs.slice(0, noteOffsetBits.size)
      
      for(b <- 0 until noteOffsetBits.size) {
        inputs(lastNoteOffsetBitsOffset + b) = offsets(b)
      }
      
      val offsetSign = outputs(outputs.size - 3)
      val rawNoteOn = outputs(outputs.size - 2)
      val rawLength = outputs(outputs.size - 1)
      
      inputs(inputs.length-3) = offsetSign
      inputs(inputs.length-2) = rawNoteOn
      inputs(inputs.length-1) = rawLength
    
      val maxRange = structure.noteOffsetRange
      
      val absOffset = MathUtil.bitsToInt(offsets) % maxRange
      
      val noteOffset = absOffset * (if(offsetSign < 0f) -1 else 1)
      val noteOn = rawNoteOn > 0.0f
      val noteLength = math.min(math.abs(rawLength).toFloat, 1f) 
      
      val realNoteLength = if(structure.fullNote) 1f else noteLength
      
      if(!noteOn) {
        None
      }
      else {
        Some((realNoteLength, noteOffset))
      }
    }
    Pattern(pattern)
  }
  
  def getPattern(org:Organism[InstrumentPatternOrganismDef.type],structure:PatternStructure, patternInput:PatternInput):Pattern = {
    val net = org.organismDefintion.expressPhenotype(org)(InstrumentPatternOrganismDef.networkConv)
    net.preRun(10)(1.0f)
    
    getPatternFromNet(net, structure, patternInput)
  }
  
}


