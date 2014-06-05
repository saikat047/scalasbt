name := "se"

version := "1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.0"

lazy val root = project.in(file(".")).aggregate(domain, util, web)

lazy val domain = project.in(file("domain"))

lazy val util = project.in(file("util"))

lazy val web = project.in(file("web")).dependsOn(util)
