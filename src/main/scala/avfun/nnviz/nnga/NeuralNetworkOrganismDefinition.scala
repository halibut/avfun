package avfun.nnviz.nnga

import avfun.nnviz.ga.OrganismDef
import avfun.nnviz.ga.ChromosomeDef

import avfun.nnviz.ga.NumericExtImplicits._
import avfun.nnviz.ga.NumericChromosomeDef

trait NeuralNetworkOrganismDefinition extends OrganismDef {
  
    def minFloatVal:Float = -1f
    def maxFloatVal:Float = 1f
    def crossoversPerChromosome:Int = 2 
  
    def floatMutationMagnitude:Float = (maxFloatVal - minFloatVal) * .25f 
    
    def connectionIndChromosomeMutateRatio = 0.2f
    def connectionWeightChromosomeMutateRatio = 0.5f
}

class ConnectionWeightChromosomeDef(override val name:String, override val numGenes:Int, 
    val networkDef:NeuralNetworkOrganismDefinition) extends NumericChromosomeDef[Float] {
  
  override def minVal:Float = networkDef.minFloatVal
  override def maxVal:Float = networkDef.maxFloatVal
  override def crossoverTimes:Int = networkDef.crossoversPerChromosome 

  override def mutateMagnitude:Float = networkDef.floatMutationMagnitude
  override def mutateRatio:Float = networkDef.connectionWeightChromosomeMutateRatio
}

class ConnectionIndexChromosomeDef(override val name:String, override val numGenes:Int, val totalNeurons:Int, 
    val networkDef:NeuralNetworkOrganismDefinition) extends NumericChromosomeDef[Int] {
  
  override def minVal:Int = 0
  override def maxVal:Int = totalNeurons - 1
  override def crossoverTimes:Int = networkDef.crossoversPerChromosome 

  override def mutateMagnitude:Int = totalNeurons
  override def mutateRatio:Float = networkDef.connectionIndChromosomeMutateRatio
}