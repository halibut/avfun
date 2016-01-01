package avfun.supercollider

import avfun.nnviz.ga.OrganismDef
import avfun.nnviz.ga.Organism

object FastOrganismTrainer {
  
  def train(oDef:OrganismDef,size:Int,generations:Int)(fitnessFunc: Organism[oDef.type] => Float):Seq[Organism[oDef.type]] = {
  
    var _orgs = for(i <- 0 until size) yield oDef.randomize 
  
    def getOrganism(orgs:Seq[Organism[oDef.type]]):Organism[oDef.type] = {
      val oSize = orgs.size
      val o1 = orgs((math.random * oSize).toInt)
      val o2 = orgs((math.random * oSize).toInt)
      val o3 = oDef.mate(o1, o2)
      o3
    }
    
    for(i <- 0 until generations) {
      val orgsWithFit = _orgs.map(o => (o, fitnessFunc(o)))
      
      val topOrgs = orgsWithFit.sortBy(_._2).map(_._1).reverse.take(size / 3)
      
      val newOrgs = for(j <- 0 until (size - topOrgs.size)) yield {
        getOrganism(topOrgs)
      }
    
      val sortedFit = orgsWithFit.sortBy(_._2).reverse.map(_._2.toString())
      val printableList = sortedFit.take(5) ++ Seq("...") ++ sortedFit.takeRight(5) 
      println(s"//${oDef.getClass.getSimpleName} - Gen $i - ${printableList}")
      
      _orgs = topOrgs ++ newOrgs
    }
    
    _orgs
    
  }
  
   
  
  
  
  
  
}