package cc.lemieux.brainfuck

import scala.io.Source

object Main {

  def list = false

  def main(args: Array[String]): Unit =
    if (args.length == 2) {
      val brainfuckProgramSource = Source.fromFile(args(0)).getLines().mkString("\n")
      val ramSize = args(1).toInt
      val program = Compiler().compile(brainfuckProgramSource)
      if (list)
        program.zipWithIndex.foreach { case (instruction, pc) => println(instruction.at(pc)) }
      CPU(ramSize).run(program)
    } else
      throw new BrainfuckException("Invalid arguments.")
}
