name := "avfun"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11+"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "com.github.wendykierp" % "JTransforms" % "3.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)


//ECLIPSE Classpath settings
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource