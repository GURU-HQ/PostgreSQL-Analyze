name := "PostgreSQL-Analyze"

version := "0.9"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
			"ch.qos.logback" % "logback-classic" % "1.0.13",
			"ch.qos.logback" % "logback-core" % "1.0.13",
			"org.slf4j" % "slf4j-api" % "1.7.5",
            "com.typesafe" % "scalalogging-slf4j_2.10" % "1.0.1",
            "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

