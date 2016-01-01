package avfun.nnviz.ga

abstract class Simulation[T <: OrganismDef](_persistence:Persistence[T], val simulationDef:SimulationDef[T]) {
  
  type oType = simulationDef.organismDefinition.type
  
  val persistence = _persistence.asInstanceOf[Persistence[oType]] 
  
  private var _currentGeneration:Generation[oType] = null
  
  protected def createNewGeneration(prevGeneration:Option[Generation[oType]],populationSize:Int):Generation[oType] = {
    val orgs = for(i <- 0 until populationSize) yield {
      createNewOrganism(prevGeneration)
    }
    
    val newGenId = if(_currentGeneration == null) 1 else _currentGeneration.generationId+1
    
    val newGen = new Generation(newGenId, orgs)
    
    persistence.saveGeneration(newGen)
    
    newGen
  }
  
  protected def initializeFitness(organism:Organism[oType]): Float
  
  protected def createFirstGenOrganism:(Organism[oType],Float) = {
    val organism = simulationDef.organismDefinition.randomize
    val fitness = initializeFitness(organism)
    (organism,fitness)
  }
  
  protected def createNthGenOrganism(previousGeneration:Generation[oType]):(Organism[oType],Float)
  
  protected def createNewOrganism(prevGeneration:Option[Generation[oType]]):(Organism[oType],Float) = {
    prevGeneration match {
      case Some(gen) => createNthGenOrganism(gen)
      case None => createFirstGenOrganism
    }
  }
  
  protected def trainGeneration
  
  def simulate(createNewGenFunc:(Generation[oType])=>Boolean,stopFunc:()=>Boolean) = {
    
    _currentGeneration = persistence.getNewestGeneration
        .getOrElse(createNewGeneration(None,simulationDef.populationSize))
        .asInstanceOf[Generation[oType]]
    
    simulationDef.organismDefinition.resetMaxId(persistence.getMaxOrganismId)
    
    do{
      
      do{
        trainGeneration
      }while(!createNewGenFunc(_currentGeneration))
      
      persistence.saveGeneration(_currentGeneration)
      
      val newGen = createNewGeneration(Some(_currentGeneration), simulationDef.populationSize)
      _currentGeneration = newGen
      persistence.saveGeneration(_currentGeneration)
      
    }while(!stopFunc())
      
  }
  
}

case class SimulationDef[T <: OrganismDef](populationSize:Int, organismDefinition:T)