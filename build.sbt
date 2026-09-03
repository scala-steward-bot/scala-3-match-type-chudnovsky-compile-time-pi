scalaVersion := "3.9.0"

val core = project

val chudnovsky = project.dependsOn(core)
