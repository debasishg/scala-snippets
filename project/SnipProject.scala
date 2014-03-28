import sbt._
import Keys._

object SnipProject extends Build
{
  lazy val root = Project("snip", file(".")) settings(coreSettings : _*)

  lazy val commonSettings: Seq[Setting[_]] = Seq(
    organization := "net.debasishg",
    version := "0.0.1",
    scalaVersion := "2.10.3",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:postfixOps")
  )

  lazy val coreSettings = commonSettings ++ Seq(
    name := "snip",

    libraryDependencies <<= scalaVersion { v =>
    if (v contains "2.10")
      Seq(
        "org.scala-lang" % "scala-reflect" % "2.10.3",
        "junit"              % "junit"               % "4.8.1"   % "test",
        "org.scalatest"      % "scalatest_2.10.0"    % "2.0.M5"  % "test",
        "org.scalaz"         % "scalaz-core_2.10"    % "7.1.0-M6",
        "org.scalaz"         % "scalaz-effect_2.10"  % "7.1.0-M6")
    else
      Seq(
        "junit"          % "junit"         % "4.8.1"  % "test",
        "org.scalatest"  % "scalatest_2.9.1" % "1.6.1" % "test")
    },

    parallelExecution in Test := false,
    publishTo <<= version { (v: String) => 
      val nexus = "https://oss.sonatype.org/" 
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2") 
    },
    credentials += Credentials(Path.userHome / ".sbt" / "sonatype.credentials"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { repo => false },
    pomExtra := (
      <url>https://github.com/debasishg/scala-snippets</url>
      <licenses>
        <license>
          <name>Apache 2.0 License</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:debasishg/scala-redis.git</url>
        <connection>scm:git:git@github.com:debasishg/scala-snippets.git</connection>
      </scm>
      <developers>
        <developer>
          <id>debasishg</id>
          <name>Debasish Ghosh</name>
          <url>http://debasishg.blogspot.com</url>
        </developer>
      </developers>),
    unmanagedResources in Compile <+= baseDirectory map { _ / "LICENSE" }
  )
}
