// SPDX-License-Identifier: BSD-3-Clause

organization := "edu.berkeley.cs"
name := "mdf"
version := "0.1-SNAPSHOT"
scalaVersion := "2.13.6"
crossScalaVersions := Seq("2.13.6", "2.12.14")
scalacOptions := Seq("-deprecation", "-feature", "-language:reflectiveCalls")
libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.9.2",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)
