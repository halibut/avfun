name := "avfun"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11+"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "com.github.wendykierp" % "JTransforms" % "3.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

libraryDependencies ++= Seq(
	"com.propensive" %% "rapture-json" % "2.0.0-M3",
	"com.propensive" %% "rapture-json-spray" % "2.0.0-M3"
	)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

//ECLIPSE Classpath settings
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource