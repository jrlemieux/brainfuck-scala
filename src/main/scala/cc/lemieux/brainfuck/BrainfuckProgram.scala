package cc.lemieux.brainfuck

import cc.lemieux.brainfuck.CPU.Instruction

case class BrainfuckProgram(instructions: IndexedSeq[Instruction]) {

  def list(): Unit = {
    println()
    instructions.zipWithIndex.foreach((instruction, pc) => println(instruction.at(pc)))
  }

  def run(ramSize: Int, debug: Boolean): Unit = CPU(ramSize, debug).run(this)
}
