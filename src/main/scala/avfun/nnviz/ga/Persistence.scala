package avfun.nnviz.ga

import java.io.OutputStream
import java.io.InputStream

trait Persistence[T <: OrganismDef] {

  import Generation.{OrganismMetaData,GenerationMetaData}
  
  val organismDef:T
  
  type oType = organismDef.type 
  
  def saveOrganism(organism:Organism[oType], generation:Generation[oType])
  
  def saveGeneration(generation:Generation[oType])
  
  def saveGenerationMetadata(generationMetadata:GenerationMetaData)
  
  def getNewestGenerationMetadata: Option[GenerationMetaData]
  
  def getNewestGeneration: Option[Generation[oType]]

  def getMaxOrganismId:Long
  
  def attachDataToOrganism(organism:OrganismMetaData, gen:GenerationMetaData, attachName:String, is:InputStream)
  
  def getAttachedData(organism:OrganismMetaData, gen:GenerationMetaData, attachName:String, os:OutputStream):Boolean
  
}

