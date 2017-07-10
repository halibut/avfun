package avfun.visualizer

import avfun.audio.stream.StreamData

case class AudioFrameData(
    framesPerSecond:Int,
    samplesPerFrame:Int,
    samplesPerSecond:Int,
    channels:Int,
    audioData:StreamData, 
    endOfData:Boolean,
    streamPosition:Option[Float])
    
    