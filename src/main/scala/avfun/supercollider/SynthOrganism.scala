package avfun.supercollider

import avfun.nnviz.ga.OrganismDef
import avfun.nnviz.ga.ChromosomeDef
import avfun.nnviz.ga.NumericChromosomeDef
import avfun.nnviz.ga.NumericExtImplicits._
import avfun.nnviz.ga.Organism
import avfun.nnviz.ga.Chromosome


object SynthOrganism {
  
  val synthGenes = Seq(
      "lfo_amp_on", "lfo_amp_speed", "lfo_amp_amt",
      "lfo_freq_on", "lfo_freq_speed", "lfo_freq_amt", 
      "synth_var_1", "synth_var_2", "synth_var_3", "synth_var_4", "synth_var_5", "synth_var_6", "synth_var_7", "synth_var_8"
      )
  
  val lfoAmpInd = 0
  val lfoGenes = 3
  val lfoFreqInd = lfoAmpInd + lfoGenes
  val varsInd = lfoFreqInd + lfoGenes 
  val numVars = 8
  
  
  val maxLfoSpeed = 20f
  val maxLfoAmp = 0.5f
  
  val uGenGenes = Seq("on", 
      "amp", "detune_on", "detune_amt", "octave_up", "octave_down",
      "type_b1", "type_b2", "type_b3", "type_b4",
      "filter_on", "filter_type_b1", "filter_type_b2", "filter_type_b3", "filter_type_b4",
      "env_perc", "env_adsr",
      "use_gen_vars", "use_synth_vars",
      "gen_var_1", "gen_var_2", "gen_var_3", "gen_var_4", "gen_var_5", "gen_var_6", "gen_var_7", "gen_var_8"
      ) 
  
  val ugAmpInd = 1
  val ugDetuneOnInd = ugAmpInd + 1
  val ugDetuneInd = ugDetuneOnInd + 1
  val ugOctvUpInd = ugDetuneInd + 1
  val ugOctvDnInd = ugOctvUpInd + 1
  val ugTypeInd = ugOctvDnInd + 1
  val ugTypeBits = 4
  val ugFilterOnInd = ugTypeInd + ugTypeBits
  val ugFilterBitsInd = ugFilterOnInd + 1
  val ugFilterBits = 4
  val ugEnvPercInd = ugFilterBitsInd + ugFilterBits
  val ugEnvAdsrInd = ugEnvPercInd + 1
  val ugUseGenVarsInd = ugEnvAdsrInd + 1
  val ugUseSynthVarsInd = ugUseGenVarsInd + 1
  val ugGenVarsInd = ugUseSynthVarsInd + 1
  val ugGenVars = numVars
      
  object SynthOrganismDef extends OrganismDef {
    
    override val chromosomeDefs: Seq[ChromosomeDef[_]] = Seq(
        SynthChromosomeDef,
        UGenChromosomeDef,
        UGenChromosomeDef,
        UGenChromosomeDef,
        UGenChromosomeDef)
  
    override val name: String = "Song Structure Organism"
  }
  
  object SynthChromosomeDef extends NumericChromosomeDef[Float] { 
  
    override val name:String = "Structure"
    val organismDef = SynthOrganismDef
    override val crossoverTimes: Int = 3  
    override val minVal: Float = -1.0f
    override val maxVal: Float = 1.0f
    override val mutateMagnitude: Float = 0.25f
    override val mutateRatio: Float = 0.05f
    override val numGenes:Int = synthGenes.size
    
  }
  
  object UGenChromosomeDef extends NumericChromosomeDef[Float] { 
  
    override val name:String = "Structure"
    val organismDef = SynthOrganismDef
    override val crossoverTimes: Int = 3  
    override val minVal: Float = -1.0f
    override val maxVal: Float = 1.0f
    override val mutateMagnitude: Float = 0.25f
    override val mutateRatio: Float = 0.05f
    override val numGenes:Int = uGenGenes.size
    
  }
  
  def getSynthDefinition(org:Organism[SynthOrganismDef.type]): SynthDef = {
   
    val synthGenes = org.getChromosome(SynthChromosomeDef).genes
    
    val ampLfo = getLfoDefinition(synthGenes.slice(lfoAmpInd, lfoAmpInd+lfoGenes))
    val freqLfo = getLfoDefinition(synthGenes.slice(lfoFreqInd, lfoFreqInd+lfoGenes))
    val synthVars = synthGenes.slice(varsInd, varsInd + numVars)
    
    val uGenDefs = org.getChromosomes(UGenChromosomeDef).map(u => getUgenDefinition(u.genes, synthVars))
    
    SynthDef(ampLfo, freqLfo, uGenDefs, synthVars)
  }
  
  private def getLfoDefinition(lfoGenes: Seq[Float]): Option[LFODef] = {
    if(lfoGenes(0) < 0.0) {
      None
    }
    else {
      val speed = math.abs(lfoGenes(1)) * maxLfoSpeed
      val amt = math.abs(lfoGenes(2)) * maxLfoAmp
      Some(LFODef(speed, amt))
    }
  }
  
  private def getUgenDefinition(uGenGenes: Seq[Float], synthVars:Seq[Float]): Option[UGenDef] = {
    if(uGenGenes(0) < 0.0) {
      None
    }
    else {
      val amp = 0.6f + 0.4f * math.abs(uGenGenes(ugAmpInd))
      val detune = if(uGenGenes(ugDetuneOnInd) > 0f) 0.02f * uGenGenes(ugDetuneInd) else 0f
      val octaveUp = if(uGenGenes(ugOctvUpInd) > 0f) 2f else 1f
      val octaveDn = if(uGenGenes(ugOctvDnInd) > 0f) 0.5f else 1f
      val freqMult = (octaveDn * octaveUp) + detune
      val ugenType = MathUtil.bitsToInt(uGenGenes.slice(ugTypeInd, ugTypeInd + ugTypeBits))
      val filterType = if(uGenGenes(ugFilterOnInd) < 0f) {
          None
        }
        else {
          Some(MathUtil.bitsToInt(uGenGenes.slice(ugFilterBitsInd, ugFilterBitsInd + ugFilterBits)))
        }
      val envType = if(uGenGenes(ugEnvAdsrInd) >= uGenGenes(ugEnvPercInd)) EnvelopeASDR() else EnvelopePerc()
      val vars = if(uGenGenes(ugUseGenVarsInd) >= uGenGenes(ugUseSynthVarsInd)) {
          uGenGenes.slice(ugGenVarsInd, ugGenVarsInd + ugGenVars)
        }
        else {
          synthVars
        }
      
      Some(UGenDef(ugenType, amp, freqMult, envType, filterType, vars))
    }
  }
  
}


