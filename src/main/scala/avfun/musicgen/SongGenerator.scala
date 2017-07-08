package avfun.musicgen

import avfun.supercollider.integ.SCClient
import avfun.supercollider.integ.SCClient
import java.io.File
import rapture.json._
import rapture.json.jsonBackends.spray._
import rapture.json.formatters.humanReadable._


object SongGenerator extends App {
  
  val songGeneratorConfig = SongGeneratorConfig.getConfigFromFile("./song-generator-config.json")
  
  val songUtil = new SongGeneratorUtil(
    baseDir = songGeneratorConfig.projectDir, songs = 20, 
    songStructureOrganisms = 20, patternOrganisms = 20, synthOrganisms = 40, instrumentPatternOrganisms = 40  
  )
  
  val scDir = new File(songGeneratorConfig.superColliderInstallDir)
  val scClient = new SCClient(scDir, new File("/tmp/sc-tmp/"))
  
  try{
    scClient.start(System.out)
    scClient.waitForBoot();
    
    songUtil.createNextGenSongs(scClient)
  }
  finally{
    scClient.quit()
  }
  
}
