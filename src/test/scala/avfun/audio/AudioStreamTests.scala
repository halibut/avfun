package avfun.audio

import org.scalatest.FunSuite
import avfun.audio.stream.AudioInputStreamAudioStream

/**
 * @author Kyle
 */
class AudioInputStreamAudioStreamTests extends FunSuite {
  

  val negByteVal:Long = -92  //Would be 164 unsigned 
  
  test("Single Byte Conversion - Unsigned"){
    val lVal = AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(Array(negByteVal.toByte), 0, 1, false)
    assert(lVal == 164)
    
    val lVal2 = AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(Array((negByteVal+1).toByte), 0, 1, false)
    assert(lVal2 == 165)
  }
  
  test("Single Byte Conversion - Signed"){
    val lVal = AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(Array(negByteVal.toByte), 0, 1, true)
    assert(lVal == -92)
    
    val lVal2 = AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(Array((negByteVal+1).toByte), 0, 1, true)
    assert(lVal2 == -91)
  }
  
  test("2 Byte Conversion - Unsigned"){
    val bArray = Array(negByteVal.toByte, 1.toByte)
    val lVal = AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(bArray, 0, 2, false)
    assert(lVal == 41985)
    
    val bArray2 = Array(negByteVal.toByte, 2.toByte)
    val lVal2 = AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(bArray2, 0, 2, false)
    assert(lVal2 == 41986)
  }
  
  test("2 Byte Conversion - Signed"){
    val bArray = Array(negByteVal.toByte, 1.toByte)
    val lVal = AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(bArray, 0, 2, true)
    assert(lVal == -23551)
    
    val bArray2 = Array(negByteVal.toByte, 2.toByte)
    val lVal2 = AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(bArray2, 0, 2, true)
    assert(lVal2 == -23550)
  }
  

  
}