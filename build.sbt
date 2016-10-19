name         := "sysson-experiments"
version      := "0.1.0-SNAPSHOT"
organization := "at.iem.sysson"
description  := "Various prototyping for SysSon"
homepage     := Some(url("https://github.com/iem-projects/sysson-experiments"))
licenses     := Seq("gpl" -> url("https://www.gnu.org/licenses/gpl-3.0.txt"))
scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint")

lazy val syssonVersion        = "1.10.3-SNAPSHOT"
lazy val scalaColliderVersion = "1.21.0"
lazy val ugensVersion         = "1.16.0"
lazy val fileUtilVersion      = "1.1.2"
lazy val equalVersion         = "0.1.1"
lazy val swingPlusVersion     = "0.2.1"
lazy val dotVersion           = "0.3.0"
lazy val traceVersion         = "0.1.0"

libraryDependencies ++= Seq(
  "at.iem"   %% "sysson"                     % syssonVersion,         // GPL !
  "de.sciss" %% "scalacollider"              % scalaColliderVersion,
  "de.sciss" %% "scalacolliderugens-api"     % ugensVersion,
  "de.sciss" %% "scalacolliderugens-plugins" % ugensVersion,
  "de.sciss" %% "fileutil"                   % fileUtilVersion,
  "de.sciss" %% "equal"                      % equalVersion,
  "de.sciss" %% "swingplus"                  % swingPlusVersion,
  "at.iem"   %% "scalacollider-dot"          % dotVersion,
  "at.iem"   %% "scalacollider-trace"        % traceVersion
)
