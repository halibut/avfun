package avfun.pipeline

class PipelineEnd[T](override val maxQueueSize:Int = 10) extends Consumer[T]{

  var blocking = true
  
  override def put(input:T) = {
    if(blocking){
      super.put(input)
    }
  }
}