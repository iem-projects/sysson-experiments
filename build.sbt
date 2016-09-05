name         := "sysson-experiments"
version      := "0.1.0-SNAPSHOT"
organization := "at.iem.sysson"
description  := "Various prototyping for SysSon"
homepage     := Some(url("https://github.com/iem-projects/sysson-experiments"))
licenses     := Seq("lgpl" -> url("https://www.gnu.org/licenses/lgpl-2.1.txt"))
scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")

lazy val scalaColliderVersion = "1.20.0"
lazy val ugensVersion         = "1.15.2"
lazy val fileUtilVersion      = "1.1.1"
lazy val dotVersion           = "0.2.0"

libraryDependencies ++= Seq(
  "de.sciss" %% "scalacollider"          % scalaColliderVersion,
  "de.sciss" %% "scalacolliderugens-api" % ugensVersion,
  "de.sciss" %% "fileutil"               % fileUtilVersion,
  "at.iem"   %% "scalacollider-dot"      % dotVersion
)
