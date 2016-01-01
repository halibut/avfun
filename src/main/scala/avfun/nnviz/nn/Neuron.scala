package avfun.nnviz.nn

import scala.collection.mutable.ArrayBuffer

class Neuron {

  protected var _curVal:Float = 0
  protected var _nextVal:Float = 0
  
  protected val _inputs = ArrayBuffer[NeuronInput]()
  
  def curVal:Float = _curVal
  
  def calcIntermediateVal(activationFunc:ActivationFunc){
    val sumIns = sumInputs(_inputs)
    val activated = activationFunc.activate(sumIns) 
    _nextVal = activated
  }
  def updateNeuronVal(){
    _curVal = _nextVal
  }
  
  def addInput(input:NeuronInput){
    _inputs += input
  }
  
  protected def sumInputs(buf:ArrayBuffer[NeuronInput]):Float = {
    var sum = 0.0f
    var i = 0
    while(i < buf.size){
      val input = buf(i)
      
      sum += input.neuron.curVal * input.weight
      
      i += 1
    }
    
    sum
  }
  
}