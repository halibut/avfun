package avfun.video

import javax.imageio.ImageIO

object test {
  
  val suffixes = ImageIO.getWriterFileSuffixes    //> suffixes  : Array[String] = Array(jpg, bmp, wbmp, jpeg, png, gif)
  
  val readers = for(suffix <- suffixes) yield {
    println(suffix)
    
    val itr = ImageIO.getImageWritersBySuffix(suffix)
    while(itr.hasNext()){
      println("  "+itr.next())
    }
  }                                               //> jpg
                                                  //|   com.sun.imageio.plugins.jpeg.JPEGImageWriter@65a6bc76
                                                  //| bmp
                                                  //|   com.sun.imageio.plugins.bmp.BMPImageWriter@23cb2378
                                                  //| wbmp
                                                  //|   com.sun.imageio.plugins.wbmp.WBMPImageWriter@740355a4
                                                  //| jpeg
                                                  //|   com.sun.imageio.plugins.jpeg.JPEGImageWriter@1ab95774
                                                  //| png
                                                  //|   com.sun.imageio.plugins.png.PNGImageWriter@6244b22e
                                                  //| gif
                                                  //|   com.sun.imageio.plugins.gif.GIFImageWriter@444c742
                                                  //| readers  : Array[Unit] = Array((), (), (), (), (), ())
  
  
}