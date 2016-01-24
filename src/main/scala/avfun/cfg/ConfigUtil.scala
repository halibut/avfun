package avfun.cfg

import rapture.json._
import rapture.json.jsonBackends.spray._
import rapture.json.formatters.humanReadable._
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.OpenOption

object ConfigUtil {
  
  val cfg = AppConfig(FileLocation("1"), FileLocation("2"), SuperColliderConfig(FileLocation("3")))
  
  def save(appConfig:AppConfig, cfgPath:Path = Paths.get("./config.json")):Unit = {
    val json:Json = Json(appConfig)
    
    val fmtJson = Json.format(json)
    
    val afterSaveAction:()=>Unit = 
      if(Files.exists(cfgPath)) {
        val backup = Paths.get(cfgPath.toFile().getName + ".bak")
        Files.copy(cfgPath, backup)
        Files.delete(cfgPath)
        ()=>{Files.delete(backup)}
      } else {
        ()=>{}
      }
    
    Files.write(cfgPath, fmtJson.getBytes)
    
    afterSaveAction()
  }
  
  def load(cfgPath:Path = Paths.get("./config.json")):AppConfig = {
    
    val json = new String(Files.readAllBytes(cfgPath))
    
    Json.parse(json).as[AppConfig]
  }

}