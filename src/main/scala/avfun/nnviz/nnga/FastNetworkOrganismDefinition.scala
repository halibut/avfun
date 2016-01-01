package avfun.nnviz.nnga

import avfun.nnviz.ga.OrganismDef
import avfun.nnviz.nn.ActivationFunc
import avfun.nnviz.nn.LinearSaturationActivation
import avfun.nnviz.ga.NumericChromosomeDef
import avfun.nnviz.ga.NumericExtImplicits
import avfun.nnviz.ga.ChromosomeDef
import avfun.nnviz.ga.PhenotypeExpression
import avfun.nnviz.nn.FastNetwork
import avfun.nnviz.ga.Organism

class FastNetworkOrganismDefinition(override val name:String, 
    val numInputs:Int, val numOutputs:Int, val numHiddenNeurons:Int, val numConnections:Int, 
    val activationFunc:ActivationFunc = LinearSaturationActivation) extends NeuralNetworkOrganismDefinition {
  
  import NumericExtImplicits._
  
  val totalNeurons = numInputs + numOutputs + numHiddenNeurons
  
  val neuronConnectionsFromIndicesChromosome = 
    new ConnectionIndexChromosomeDef("From Connection Indices", numConnections, totalNeurons, this)
  
  val neuronConnectionsToIndicesChromosome = 
    new ConnectionIndexChromosomeDef("To Connection Indices", numConnections, totalNeurons, this)
  
  val neuronConnectionWeightsChromosome = 
    new ConnectionWeightChromosomeDef("Connection Weights", numConnections, this)
  
  override val chromosomeDefs:Seq[ChromosomeDef[_]] = Seq(
    neuronConnectionsFromIndicesChromosome,
    neuronConnectionsToIndicesChromosome,
    neuronConnectionWeightsChromosome
  ) 
  
}

object FastNetworkOrganismDefinition{
  
  implicit def implicitlyGetFastNetworkConverter[T <: FastNetworkOrganismDefinition](orgDef:T):PhenotypeExpression[T,FastNetwork] = {
    new PhenotypeExpression[T,FastNetwork]{
      def expressPhenotype(organism: Organism[T]): FastNetwork = {
      
        val nnDef = organism.organismDefintion
        
        val fromIndices = organism.getChromosome(nnDef.neuronConnectionsFromIndicesChromosome).genes
        val toIndices = organism.getChromosome(nnDef.neuronConnectionsToIndicesChromosome).genes
        val connWeights = organism.getChromosome(nnDef.neuronConnectionWeightsChromosome).genes
        
        val connections = (fromIndices,toIndices,connWeights)
          .zipped
          .map{case (from,to,w) => new FastNetwork.Conn(from,to,w) }
        
        new FastNetwork(nnDef.numInputs, nnDef.numOutputs, nnDef.numHiddenNeurons, connections, nnDef.activationFunc)
      }
    }
  }
  
//  implicit object FastNetworkPhenotypeExpression extends PhenotypeExpression[FastNetworkOrganismDefinition,FastNetwork] {
//    def expressPhenotype(organism: Organism[FastNetworkOrganismDefinition]): FastNetwork = {
//      
//      val nnDef = organism.organismDefintion
//      
//      val fromIndices = organism.getChromosome(nnDef.neuronConnectionsFromIndicesChromosome).genes
//      val toIndices = organism.getChromosome(nnDef.neuronConnectionsToIndicesChromosome).genes
//      val connWeights = organism.getChromosome(nnDef.neuronConnectionWeightsChromosome).genes
//      
//      val connections = (fromIndices,toIndices,connWeights)
//        .zipped
//        .map{case (from,to,w) => new FastNetwork.Conn(from,to,w) }
//      
//      new FastNetwork(nnDef.numInputs, nnDef.numOutputs, nnDef.numHiddenNeurons, connections, nnDef.activationFunc)
//    }
//  }
}