package avfun.nnviz.ga

/**
 * @author Kyle
 */
trait PhenotypeExpression[G <: OrganismDef,P] {
  
  def expressPhenotype(organism:Organism[G]):P
}