name         := "sysson-experiments"
version      := "0.1.0-SNAPSHOT"
organization := "at.iem.sysson"
description  := "Various prototyping for SysSon"
homepage     := Some(url("https://github.com/iem-projects/sysson-experiments"))
licenses     := Seq("gpl" -> url("https://www.gnu.org/licenses/gpl-3.0.txt"))
scalaVersion := "2.12.1"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint")

lazy val syssonVersion        = "1.13.0"
lazy val scalaColliderVersion = "1.22.3"
lazy val ugensVersion         = "1.16.4"
lazy val fscapeVersion        = "2.6.3"
lazy val fileUtilVersion      = "1.1.2"
lazy val equalVersion         = "0.1.2"
lazy val swingPlusVersion     = "0.2.2"
lazy val dotVersion           = "0.4.1"
lazy val traceVersion         = "0.2.1"
lazy val pdflitzVersion       = "1.2.2"
lazy val orsonpdfVersion      = "1.7"

libraryDependencies ++= Seq(
  "at.iem"   %% "sysson"                     % syssonVersion,         // GPL !
  "de.sciss" %% "scalacollider"              % scalaColliderVersion,
  "de.sciss" %% "scalacolliderugens-api"     % ugensVersion,
  "de.sciss" %% "scalacolliderugens-plugins" % ugensVersion,
  "de.sciss" %% "fscape-core"                % fscapeVersion,
  "de.sciss" %% "fscape-lucre"               % fscapeVersion,
  "de.sciss" %% "fileutil"                   % fileUtilVersion,
  "de.sciss" %% "equal"                      % equalVersion,
  "de.sciss" %% "swingplus"                  % swingPlusVersion,
  "at.iem"   %% "scalacollider-dot"          % dotVersion,
  "at.iem"   %% "scalacollider-trace"        % traceVersion,
  "de.sciss" %% "pdflitz"                    % pdflitzVersion,
  "com.orsonpdf" % "orsonpdf"                % orsonpdfVersion   // try as alternative to iText PDF
)
