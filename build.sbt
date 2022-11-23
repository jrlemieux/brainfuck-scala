
name := "brainfuck"

version := "1.0"

scalaVersion := "3.2.0"

assembly / test := {}

assembly / assemblyMergeStrategy := {
  case p @ PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => (assembly / assemblyMergeStrategy).value(x)
}

assembly / mainClass := Some("cc.lemieux.brainfuck.Main")

