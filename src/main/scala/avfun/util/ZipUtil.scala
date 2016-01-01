package avfun.util

import java.io.OutputStream
import java.util.zip.ZipOutputStream
import java.io.InputStream
import java.io.ByteArrayOutputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream
import java.io.FileNotFoundException
import java.io.ByteArrayInputStream

/**
 * Note: the zip method is inherently different from the unzip methods because it keeps
 * the returned ZipOutputStream open and in a state where more entries can be written. 
 * 
 * This is because a ZipOutpuStream can be further written to, while a normal ZipInputStream 
 * cannot be repositioned to an earlier entry once it has been traversed. 
 * 
 * None of the methods in this object close the Input/Output Streams, so it is important to 
 * close the ZipOutputStream after you are done adding entries or close/reset/recreate any
 * InputStreams after reading. 
 */

object ZipUtil {

  /**
   * Adds the specified file data to a ZipOutputStream with the specified fileName/entryName
   * 
   * @param fileData The data to compress and add to the zip archive
   * 
   * @param fileName The entry name to use for adding to the zip archive
   * 
   * @param os The underlying OutputStream that the ZipOutputStream will write to. If os itself is
   * a ZipOutputStream, then it will simply add the entry to this stream. Otherwise, a new
   * ZipOutputStream will be created that wraps the specified os:OutputStream
   * 
   * @return ZipOutputStream which can be used to pass as the os argument into additional zip
   * method calls, or can simply be closed if no additional compressing is needed
   */
  def zip(fileData:InputStream,fileName:String,os:OutputStream = new ByteArrayOutputStream):ZipOutputStream = {
    val zos = os match{
      case z:ZipOutputStream => z
      case _ => new ZipOutputStream(os)
    }
    
    zos.putNextEntry(new ZipEntry(fileName))
    
    StreamUtil.copyData(fileData, zos)
    
    zos.closeEntry()
    zos
  }
  
  
  /**
   * Provides an iterator-like interface to the entries in a ZipInputStream.
   *
   * @param zippedData InputStream of zipped data to read from. If zippedData itself is an instance of
   * ZipInputStream, then it is expected that the current entry/position of the stream is set prior to
   * this method being invoked, and will treat it as a normal InputStream
   * 
   * @param entryFunc is a function that takes a ZipEntry, and the InputStream of data
   * for that ZipEntry.
   * 
   */
  def forEachZipEntry(zippedData:InputStream)(entryFunc:(ZipEntry,InputStream)=>Unit):Unit = {
    val zis = new ZipInputStream(zippedData)
    
    var zEntry = zis.getNextEntry
    while(zEntry != null){
      
      entryFunc(zEntry, zis)
      
      zEntry = zis.getNextEntry
    }
  }
  
  /**
   * Opens an input stream (assumed to be Zip-ed data) and finds the specified entry.
   * Writes the entry contents to the output stream.
   * 
   * @param zippedData InputStream of zipped data to read from. If zippedData itself is an instance of
   * ZipInputStream, then it is expected that the current entry/position of the stream is set prior to
   * this method being invoked, and will treat it as a normal InputStream
   * 
   * @throws FileNotFoundException if the entry could not be found
   */
  def unzipSingleEntryToOutputStream(zippedData:InputStream,entryName:String,os:OutputStream):Unit = {
   var zEntry:ZipEntry = null
    forEachZipEntry(zippedData){case (entry,is) =>
      if(entry.getName == entryName){
        zEntry = entry
        StreamUtil.copyData(is,os)
      }
    }
    
    if(zEntry == null){
      throw new FileNotFoundException(s"Could not find entry named ${entryName} in input stream.")
    }
  }
  
}