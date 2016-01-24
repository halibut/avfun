package avfun.cfg

import java.io.File
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.Paths

case class AppConfig(
  tmpDir:FileLocation,
  outputDir:FileLocation,
  superCollider:SuperColliderConfig
)

case class SuperColliderConfig(
  installDir:FileLocation
)

case class FileLocation(loc:String) {
  lazy val path = Paths.get(loc)
  def absolutePath = path.toAbsolutePath()
}
