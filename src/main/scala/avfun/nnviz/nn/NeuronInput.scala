package avfun.nnviz.nn

class NeuronInput(val neuron:Neuron, inputWeight:Float = 0) {

  private var _weight:Float = inputWeight
  
  def weight:Float = _weight
  def weight_=(w:Float):Unit = _weight = w
  
}