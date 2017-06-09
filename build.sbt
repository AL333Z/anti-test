import com.typesafe.sbt.SbtGit.git
import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._
import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.{Versions, _}

name := "anti-test"

scalaVersion := "2.12.2"

organization := "com.al333z"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

description := "anti-test"

licenses := Seq(("MIT", new URL("https://opensource.org/licenses/mit-license.php")))

homepage := Some(url("http://github.com/AL333Z/anti-test"))

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0",
  "org.scalatest" %% "scalatest" % "3.0.1"
)

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ ⇒ false }

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

pomExtra :=
  <scm>
    <url>http://github.com/AL333Z/anti-test</url>
    <connection>scm:git:git://github.com/AL333Z/anti-test.git</connection>
    <developerConnection>scm:git:git@github.com:AL333Z/anti-test.git</developerConnection>
  </scm>
    <developers>
      <developer>
        <id>AL333Z</id>
        <name>Alessandro Zoffoli</name>
        <url>http://twitter.com/al333z</url>
      </developer>
      <developer>
        <id>azanin</id>
        <name>Alessandro Zanin</name>
        <url>http://twitter.com/azanin</url>
      </developer>
      <developer>
        <id>r-tomassetti</id>
        <name>Renato Tomassetti</name>
        <url>http://twitter.com/r-tomassetti</url>
      </developer>
    </developers>

val VersionRegex = "v([0-9]+.[0-9]+.[0-9]+)-?(.*)?".r

def setReleaseVersionCustom(): ReleaseStep = {
  def setVersionOnly(selectVersion: Versions => String): ReleaseStep = { st: State =>
    val vs = st.get(ReleaseKeys.versions).getOrElse(sys.error("No versions are set! Was this release part executed before inquireVersions?"))
    val selected = selectVersion(vs)
    st.log.info("Setting version to '%s'." format selected)
    val useGlobal = Project.extract(st).get(releaseUseGlobalVersion)
    val versionStr = (if (useGlobal) globalVersionString else versionString) format selected

    reapply(Seq(
      if (useGlobal) version in ThisBuild := selected
      else version := selected
    ), st)
  }

  setVersionOnly(_._1)
}

git.useGitDescribe := true
git.baseVersion := "0.0.0"
git.gitTagToVersionNumber := {
  case VersionRegex(v, "") => Some(v)
  case VersionRegex(v, s) => Some(s"$v-$s")
  case _ => None
}

releaseVersion <<= releaseVersionBump(bumper => {
  ver =>
    Version(ver)
      .map(_.withoutQualifier)
      .map(_.bump(bumper).string).getOrElse(versionFormatError)
})

releaseProcess := Seq(
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  setReleaseVersionCustom(),
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _), enableCrossBuild = true),
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true),
  pushChanges
)
