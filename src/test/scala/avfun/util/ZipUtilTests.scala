package avfun.util

import org.scalatest.FunSuite
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.util.zip.ZipOutputStream

class ZipUtilTests extends FunSuite {
  
  test("Read and write Zip data"){
    val entryName = "test"
    val data = ((0 until 100) map (x => "This is some zip data to compress.")).mkString(" ")
    val dataBytes = data.getBytes
    val dataIs = new ByteArrayInputStream(dataBytes); 
    
    val zippedDataOs = new ByteArrayOutputStream()
    val zipOs = ZipUtil.zip(dataIs,entryName,zippedDataOs)
    zipOs.close()
        
    val zippedBytes = zippedDataOs.toByteArray()
    val zippedDataIs = new ByteArrayInputStream(zippedBytes) 
    val testDataOs = new ByteArrayOutputStream()
    ZipUtil.unzipSingleEntryToOutputStream(zippedDataIs, entryName, testDataOs)
    
    val testData = new String(testDataOs.toByteArray())
    
    assert(data === testData)
  }
  
  test("Read and write multiple Zip data entries"){
    val entryName = "test"
    val data = ((0 until 100) map (x => s"[${x}]This is some zip data to compress.")).zipWithIndex
    
    val zippedDataOs = new ByteArrayOutputStream()
    val zos = new ZipOutputStream(zippedDataOs)
    
    for(d <- data){
      val (ds,di) = d
      val dataIs = new ByteArrayInputStream(ds.getBytes);
      ZipUtil.zip(dataIs,entryName+di,zos)
    }
    
    zos.close()
    
    val zippedDataIs = new ByteArrayInputStream(zippedDataOs.toByteArray()) 
    
    ZipUtil.forEachZipEntry(zippedDataIs){case (entry,is) =>
      val isBytes = StreamUtil.getAsByteArray(is)
      val str = new String(isBytes)
      
      assert(data.exists(d => d._1 == str && entry.getName() == entryName+d._2))
    }
    
  }
}