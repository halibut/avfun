package avfun.util

import java.io.File
import scala.annotation.tailrec

object FileUtil {

  def deleteIfExists(fileName:String):Boolean = {
    val file = new File(fileName)
    if(file.exists()){
      file.delete();
    }
    else{
      false
    }
  }
  
  def deleteRecursive(dir:File):Unit = {
    if(dir.isFile()){
      dir.delete()
    }
    else{
      dir.listFiles.foreach{ f =>
        deleteRecursive(f)
      }
      dir.delete()
    }
  }
}