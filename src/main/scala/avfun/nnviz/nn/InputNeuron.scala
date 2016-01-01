package avfun.nnviz.nn

import scala.collection.mutable.ArrayBuffer

class InputNeuron extends Neuron {

  private var _inputVal = 0.0f
  
  override def calcIntermediateVal(activationFunc:ActivationFunc){
    //Remove logic to set value
  }
  override def updateNeuronVal(){
    //Remove logic to set value
  }
  
  
  def setVal(v:Float){
    this._inputVal = v
  }
  override def curVal:Float = _inputVal
  
}