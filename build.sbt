name         := "sysson-experiments"
version      := "0.1.0-SNAPSHOT"
organization := "at.iem.sysson"
description  := "Various prototyping for SysSon"
homepage     := Some(url("https://github.com/iem-projects/sysson-experiments"))
licenses     := Seq("lgpl" -> url("https://www.gnu.org/licenses/lgpl-2.1.txt"))
scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")

lazy val scalaColliderVersion = "1.19.0"

libraryDependencies ++= Seq(
  "de.sciss" %% "scalacollider" % scalaColliderVersion
)
