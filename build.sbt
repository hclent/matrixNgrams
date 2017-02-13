name := "matrixNgrams"
version := "0.1.0"
scalaVersion := "2.11.6"
libraryDependencies  ++= Seq(
   "org.scalanlp" %% "breeze" % "0.12",
   "org.scalanlp" %% "breeze-natives" % "0.12",
   "org.scalanlp" %% "breeze-viz" % "0.12",
   "ai.lum" % "common_2.11" % "0.0.7",
   "org.scala-lang" % "scala-swing" % "2.10+"
)

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
