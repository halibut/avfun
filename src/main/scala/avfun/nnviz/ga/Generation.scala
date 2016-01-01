package avfun.nnviz.ga

class Generation[T <: OrganismDef](val generationId:Int, organisms:Seq[(Organism[T],Float)]){
  private var _organisms = organisms.map(x => x._1.id -> x._1).toMap
  private var _generationFitness = organisms.map(x => x._1.id -> x._2).toMap
  
  def getOrganism(id:Long):Organism[T] = {
    _organisms(id)
  }
  
  def getOrganisms:Seq[Organism[T]] = {
    _organisms.map(x => x._2).toSeq
  }
  
  def updateFitness(organismId:Long, fitness:Float){
    _generationFitness += organismId -> fitness
  }
  
  def getFitness(organismId:Long):Float = {
    _generationFitness(organismId)
  }
  
}

object Generation{

  def GenMetadata[T <: OrganismDef](gen:Generation[T]):GenerationMetaData = {
    val organisms = gen._organisms.map{case (id,org) =>
      OrganismMetaData(id, gen._generationFitness(id), org.p1Id, org.p2Id)
    }.toSeq
    
    GenerationMetaData(gen.generationId, organisms)
  }
  
  case class GenerationMetaData(id:Int, organisms:Seq[OrganismMetaData])
  case class OrganismMetaData(id:Long, fitness:Float, p1Id:Option[Long], p2Id:Option[Long])
}
