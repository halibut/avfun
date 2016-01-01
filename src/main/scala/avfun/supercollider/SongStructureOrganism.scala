package avfun.supercollider

import avfun.nnviz.ga.OrganismDef
import avfun.nnviz.ga.ChromosomeDef
import avfun.nnviz.ga.NumericChromosomeDef
import avfun.nnviz.ga.NumericExtImplicits._
import avfun.nnviz.ga.Organism


object SongStructureOrganism {
  
  val songParts = Seq("hasIntro", "hasOutro", "hasChorus", "hasBreak")
  val instrumentBits = Seq("instruments_b1", "instruments_b2", "instruments_b3", "instruments_b4")
  val noteDivBits = Seq("noteDiv_b1", "noteDiv_b2", "noteDiv_b3", "noteDiv_b4")
  
  val outputNames = songParts ++ instrumentBits ++ noteDivBits ++ Seq("beatsPerMinute") 
        
  val songPartsInd = 0
  val instrumentBitsInd = songPartsInd + songParts.size
  val noteDivBitsInd = instrumentBitsInd + instrumentBits.size
  val bpmInd = noteDivBitsInd + noteDivBits.size
  
  val MaxBPM = 180f
  val MinBPM = 60f
  
  object SongStructureOrganismDef extends OrganismDef {
    
    override val chromosomeDefs: Seq[ChromosomeDef[_]] = Seq(SongStructureChromosomeDef)
  
    override val name: String = "Song Structure Organism"
  }
  
  object SongStructureChromosomeDef extends NumericChromosomeDef[Float] { 
  
    override val name:String = "Structure"
    val organismDef = SongStructureOrganismDef
    override val crossoverTimes: Int = 3  
    override val minVal: Float = -1.0f
    override val maxVal: Float = 1.0f
    override val mutateMagnitude: Float = 0.5f
    override val mutateRatio: Float = 0.05f
    override val numGenes:Int = outputNames.size
    
  }
  
  def getSongStructure(org:Organism[SongStructureOrganismDef.type]): SongStructure = {
   
    val genes= org.getChromosome(SongStructureChromosomeDef).genes
    val hasIntro = genes(songPartsInd+0) > 0.0f
    val hasOutro = genes(songPartsInd+1) > 0.0f
    val hasChorus = genes(songPartsInd+2) > 0.0f
    val hasBreak = genes(songPartsInd+3) > 0.0f
    
    val bpm = ((MaxBPM-MinBPM) * (genes(bpmInd) / 2.0f + 0.5f)) + MinBPM    
    
    val instruments = math.max(1, MathUtil.bitsToInt(genes.slice(instrumentBitsInd, instrumentBitsInd + instrumentBits.size)))
    
    val noteDiv = 1 + MathUtil.bitsToInt(genes.slice(noteDivBitsInd, noteDivBitsInd + noteDivBits.size))
    
    SongStructure(hasIntro, hasOutro, hasChorus, hasBreak, bpm, instruments, noteDiv)
  }
}


