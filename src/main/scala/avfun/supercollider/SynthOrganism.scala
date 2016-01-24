package avfun.supercollider

import avfun.nnviz.ga.OrganismDef
import avfun.nnviz.ga.ChromosomeDef
import avfun.nnviz.ga.NumericChromosomeDef
import avfun.nnviz.ga.NumericExtImplicits._
import avfun.nnviz.ga.Organism
import avfun.nnviz.ga.Chromosome
import avfun.supercollider.model._
import avfun.nnviz.ga.Chromosome


object SynthOrganism {
  
  val synthGenes = Seq(
      "lfo_amp_on", "lfo_amp_speed", "lfo_amp_amt",
      "lfo_freq_on", "lfo_freq_speed", "lfo_freq_amt", 
      "synth_var_1", "synth_var_2", "synth_var_3", "synth_var_4"
      )
  
  val lfoAmpInd = 0
  val lfoGenes = 3
  val lfoFreqInd = lfoAmpInd + lfoGenes
  val varsInd = lfoFreqInd + lfoGenes 
  val numVars = 4
  
  val maxLfoSpeed = 20f
  val maxLfoAmp = 0.5f
  
  val uGenGenes = Seq("on", 
      "amp", "detune_on", "detune_amt", "octave_up", "octave_down",
      "freqDyn_on", "freqStatic_on", "freqStatic_amt",
      "type_b1", "type_b2", "type_b3", "type_b4",
      "gen_var_1", "gen_var_2", "gen_var_3", "gen_var_4",
      "filter_on", "filter_amt", 
      "filter_type_b1", "filter_type_b2", "filter_type_b3", "filter_type_b4",
      "filter_var_1", "filter_var_2", "filter_var_3", "filter_var_4",
      "env_type_b1", "env_type_b2", "env_type_b3",
      "env_var_1", "env_var_2", "env_var_3", "env_var_4"
      ) 
  
  val ugAmpInd = 1
  val ugDetuneOnInd = ugAmpInd + 1
  val ugDetuneInd = ugDetuneOnInd + 1
  val ugOctvUpInd = ugDetuneInd + 1
  val ugOctvDnInd = ugOctvUpInd + 1
  val ugFreqDynInd = ugOctvDnInd + 1
  val ugFreqStaticInd = ugFreqDynInd + 1
  val ugFreqStaticAmtInd = ugFreqStaticInd + 1
  
  val ugTypeInd = ugFreqStaticAmtInd + 1
  val ugTypeBits = 4
  val ugGenVarsInd = ugTypeInd + ugTypeBits
  
  val ugFilterOnInd = ugGenVarsInd + numVars
  val ugFilterAmtInd = ugFilterOnInd + 1
  val ugFilterBitsInd = ugFilterAmtInd + 1
  val ugFilterBits = 4
  val ugFilterVarsInd = ugFilterBitsInd + ugFilterBits
  
  val ugEnvTypeBitsInd = ugFilterVarsInd + numVars
  val ugEnvTypeBits = 3
  val ugEnvVarsInd = ugEnvTypeBitsInd + ugEnvTypeBits
  
      
  val minStaticFreq = 20f
  val maxStaticFreq = 12000f
  
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
  
    override val name:String = "Synth"
    val organismDef = SynthOrganismDef
    override val crossoverTimes: Int = 3  
    override val minVal: Float = 0.0f
    override val maxVal: Float = 1.0f
    override val mutateMagnitude: Float = 0.15f
    override val mutateRatio: Float = 0.15f
    override val numGenes:Int = synthGenes.size
    
  }
  
  object UGenChromosomeDef extends NumericChromosomeDef[Float] { 
  
    override val name:String = "UGen"
    val organismDef = SynthOrganismDef
    override val crossoverTimes: Int = 3  
    override val minVal: Float = 0.0f
    override val maxVal: Float = 1.0f
    override val mutateMagnitude: Float = 0.15f
    override val mutateRatio: Float = 0.15f
    override val numGenes:Int = uGenGenes.size
    
  }
  
  def getSynthDefinition(org:Organism[SynthOrganismDef.type]): SynthDef = {
   
    val synthGenes = org.getChromosome(SynthChromosomeDef).genes
    
    val ampLfo = getLfoDefinition(synthGenes.slice(lfoAmpInd, lfoAmpInd+lfoGenes))
    val freqLfo = getLfoDefinition(synthGenes.slice(lfoFreqInd, lfoFreqInd+lfoGenes))
    val synthVars = UGenVars(synthGenes.slice(varsInd, varsInd + numVars))
    
    val uGenDefs = org.getChromosomes(UGenChromosomeDef).map(u => getUgenDefinition(u.genes))
    
    SynthDef(ampLfo, freqLfo, uGenDefs, synthVars)
  }
  
  private def getLfoDefinition(lfoGenes: Seq[Float]): Option[LFODef] = {
    if(lfoGenes(0) < 0.75f) {
      None
    }
    else {
      val speed = lfoGenes(1) * maxLfoSpeed
      val amt = lfoGenes(2) * maxLfoAmp
      Some(LFODef(speed, amt))
    }
  }
  
  def getSynthOrganism(synthDef:SynthDef):Organism[SynthOrganismDef.type] = {
    val synthGenes = getGenesFromSynthDef(synthDef)
    val ugenGenes1 = getGenesFromUgenDef(synthDef.ugens(0))
    val ugenGenes2 = getGenesFromUgenDef(synthDef.ugens(1))
    val ugenGenes3 = getGenesFromUgenDef(synthDef.ugens(2))
    val ugenGenes4 = getGenesFromUgenDef(synthDef.ugens(3))
    
    new Organism(0L, Seq(
        Chromosome(SynthChromosomeDef, synthGenes),
        Chromosome(UGenChromosomeDef, ugenGenes1),
        Chromosome(UGenChromosomeDef, ugenGenes2),
        Chromosome(UGenChromosomeDef, ugenGenes3),
        Chromosome(UGenChromosomeDef, ugenGenes4)), 
        None, None)(SynthOrganismDef)
  }
  
  private def getGenesFromSynthDef(synthDef:SynthDef):Seq[Float] = {
    val genes = new Array[Float](synthGenes.size)
    
    synthDef.lfoAmp match {
      case Some(lfo) => {
        genes(lfoAmpInd) = 0.875f
        genes(lfoAmpInd+1) = lfo.speed / maxLfoSpeed
        genes(lfoAmpInd+2) = lfo.amount / maxLfoAmp
      }
      case None => {
        genes(lfoAmpInd) = 0.25f
      }
    }
    synthDef.lfoFreq match {
      case Some(lfo) => {
        genes(lfoFreqInd) = 0.875f
        genes(lfoFreqInd+1) = lfo.speed / maxLfoSpeed
        genes(lfoFreqInd+2) = lfo.amount / maxLfoAmp
      }
      case None => {
        genes(lfoFreqInd) = 0.25f
      }
    }
    
    for(i <- 0 until numVars) {
      genes(varsInd + i) = synthDef.vars.values(i)
    }
    
    genes
  }
  
  private def getGenesFromUgenDef(ugenDef:Option[UGenDef]):Seq[Float] = {
    val genes = new Array[Float](uGenGenes.size)
    
    ugenDef match {
      case None => {
        genes(0) = 0.25f 
      }
      case Some(ug) => {
        genes(0) = 0.75f
        genes(ugAmpInd) = (ug.amp - .6f) / .4f
        
        //Update the frequency type
        ug.freqControl match {
          case sf:StaticFreq => {
            genes(ugFreqDynInd) = 0.25f
            genes(ugFreqStaticInd) = 0.875f
            genes(ugFreqStaticAmtInd) = (sf.freq - minStaticFreq) / (maxStaticFreq - minStaticFreq)
          }
          case df:DynamicFreq => {
            val (octUp,octDn) = if(df.freqMult < 0.75f) {
              (0.25f, 0.75f) 
            } else if(df.freqMult > 1.5f) {
              (0.75f, 0.25f)
            } else {
              (0.25f, 0.25f)
            }
            genes(ugFreqDynInd) = 0.75f
            genes(ugFreqStaticInd) = 0.25f
            genes(ugOctvUpInd) = octUp
            genes(ugOctvDnInd) = octDn
            if(df.detuneAmt != 0f) {
              genes(ugDetuneOnInd) = 0.75f
              genes(ugDetuneInd) = (df.detuneAmt + 0.01f) / 0.02f
            }
          }
        }
      
      
        //update the UGen Type
        for(i <- 0 until ugTypeBits) {
          val b = MathUtil.intToBits(ug.uGenType.typeId, .25f, .75f)(i)
          genes(ugTypeInd + i) = b
        }
        //Update the UGen vars
        for(i <- 0 until numVars) {
          genes(ugGenVarsInd + i) = ug.uGenType.getUGenVars(i)
        }
        
        
        //Update the envelope type
        for(i <- 0 until ugEnvTypeBits) {
          val b = MathUtil.intToBits(ug.envelopeType.typeInd, .25f, .75f)(i)
          genes(ugEnvTypeBitsInd + i) = b
        }
        //Update the envelope vars
        for(i <- 0 until numVars) {
          genes(ugEnvVarsInd + i) = ug.envelopeType.uGenVals(i)
        }
        
        
        //Update the filter type
        ug.filterType match {
          case Some(f) => {
            genes(ugFilterOnInd) = 0.75f
            genes(ugFilterAmtInd) = f.filterAmt
            for(i <- 0 until ugFilterBits) { 
              val b = MathUtil.intToBits(f.typeInd, .25f, .75f)(i)
              genes(ugFilterBitsInd + i) = b
            }
            
            for(i <- 0 until numVars) {
              genes(ugFilterVarsInd + i) = f.getVars(i)
            }
          }
          case None => {
            genes(ugFilterOnInd) = 0.25f
          }
        }
        
      }
    }
    
    genes
  }
  
  private def getUgenDefinition(uGenGenes: Seq[Float]): Option[UGenDef] = {
    if(uGenGenes(0) < 0.5f) {
      None
    }
    else {
      val amp = 0.6f + 0.4f * uGenGenes(ugAmpInd)
      val detune = if(uGenGenes(ugDetuneOnInd) > 0.5f) (-0.01f + 0.02f * uGenGenes(ugDetuneInd)) else 0f
      val octaveUp = if(uGenGenes(ugOctvUpInd) > 0.5f) 2f else 1f
      val octaveDn = if(uGenGenes(ugOctvDnInd) > 0.5f) 0.5f else 1f
      val freqMult = (octaveDn * octaveUp)

      //UGen parameters
      val ugenTypeInd = MathUtil.bitsToInt(uGenGenes.slice(ugTypeInd, ugTypeInd + ugTypeBits), 0.5f)
      val uGenVars = UGenVars(uGenGenes.slice(ugGenVarsInd, ugGenVarsInd + numVars))
      val ugenType = UGenType.getType(ugenTypeInd, uGenVars)
      
      //Filter parameters
      val filterTypeInd = MathUtil.bitsToInt(uGenGenes.slice(ugFilterBitsInd, ugFilterBitsInd + ugFilterBits), 0.5f)
      val filterVars = UGenVars(uGenGenes.slice(ugFilterVarsInd, ugFilterVarsInd + numVars))
      val filterAmt = uGenGenes(ugFilterAmtInd)
      val filterType = if(uGenGenes(ugFilterOnInd) < 0.5f) {
          None
        }
        else {
          Some(FilterType.getType(filterTypeInd, filterVars, filterAmt))
        }
      
      //Env parameters
      val envTypeInd = MathUtil.bitsToInt(uGenGenes.slice(ugEnvTypeBitsInd, ugEnvTypeBitsInd + ugEnvTypeBits), 0.5f)
      val envVars = UGenVars(uGenGenes.slice(ugEnvVarsInd, ugEnvVarsInd + numVars))
      val envType = EnvelopeType.getType(envTypeInd, envVars)
        
      val freqType = if(uGenGenes(ugFreqDynInd) > 0.5f * uGenGenes(ugFreqStaticInd)) {
        DynamicFreq(freqMult, detune)
      } else {
        val staticFreq = minStaticFreq + (maxStaticFreq-minStaticFreq) * uGenGenes(ugFreqStaticAmtInd)
        StaticFreq(staticFreq)
      }
      
      Some(UGenDef(ugenType, amp, freqType, envType, filterType))
    }
  }
  
}


