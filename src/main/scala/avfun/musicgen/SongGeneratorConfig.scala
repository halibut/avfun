package avfun.musicgen

import java.nio.file.Paths
import java.nio.file.Files
import rapture.json._
import rapture.json.jsonBackends.spray._
import rapture.json.formatters.humanReadable._

case class SongGeneratorConfig(
  superColliderInstallDir: String,
  projectDir: String = "C:/tmp/song-gen/"
)

object SongGeneratorConfig{
  
  def getConfigFromFile(file:String):SongGeneratorConfig = {
    val configFile = Paths.get(file)
    
    if(!configFile.toFile().exists()) {
      throw new IllegalArgumentException(s"Config file [${file}] not found.")
    }
    else{
      val configFileStr = new String(Files.readAllBytes(configFile))
      val config = Json.parse(configFileStr).as[SongGeneratorConfig]
      config
    }
  }
  
}