package avfun.util

import java.io.InputStream
import java.io.OutputStream
import java.io.ByteArrayOutputStream

object StreamUtil {

  def copyData(is:InputStream,os:OutputStream,copyBufferSizeBytes:Int = 1024):Unit = {
    val buffer = new Array[Byte](copyBufferSizeBytes)
    var read = 0
    
    while(read >= 0){
      read = is.read(buffer)
      if(read > 0){
        os.write(buffer, 0, read)
      }
    }
  }
  
  def getAsByteArray(is:InputStream):Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    
    copyData(is,bos)
    
    bos.toByteArray()
  }
}