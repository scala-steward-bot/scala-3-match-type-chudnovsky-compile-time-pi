val common = Def.settings(
  scalaVersion := "3.8.0-RC6"
)

common

val core = project.settings(common)

val chudnovsky = project.settings(common).dependsOn(core)
