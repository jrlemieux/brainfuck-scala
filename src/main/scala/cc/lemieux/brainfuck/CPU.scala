package cc.lemieux.brainfuck

object CPU {

  trait Instruction {
    def at(index: Int) = f"$index%04d: $this"
  }

  case class IncrementPtr(by: Int) extends Instruction

  case class IncrementValue(by: Int) extends Instruction

  // var because there is a fixup when the ] is found.
  case class TestZeroAndJump(var jumpPC: Int) extends Instruction

  case class Goto(jumpPC: Int) extends Instruction

  case class ReadChar() extends Instruction

  case class PrintChar() extends Instruction

  case class Halt() extends Instruction
}

import CPU._

case class CPU(ramSize: Int, debug: Boolean = false) {

  var pc = 0
  val ram = new Array[Short](ramSize)
  var ptr = 0
  var halted = true

  def decode(instruction: Instruction) =
    instruction match {
      case IncrementPtr(by) =>
        ptr += by
        pc += 1
      case IncrementValue(by) =>
        ram(ptr) = ((ram(ptr) + by) & 0xFF).toShort
        pc += 1
      case TestZeroAndJump(jumpPC) =>
        pc = if (ram(ptr) == 0) jumpPC else pc + 1
      case Goto(jumpPC) =>
        pc = jumpPC
      case ReadChar() =>
        ram(ptr) = System.in.read().toShort
        pc += 1
      case PrintChar() =>
        System.out.print(ram(ptr).toChar)
        pc += 1
      case Halt() =>
        halted = true
    }

  def run(program: IndexedSeq[Instruction]) = {
    halted = false
    while (!halted) {
      val instruction = program(pc)
      if (debug)
        System.err.println(instruction.at(pc))
      val decodePC = pc
      try
        decode(instruction)
      catch {
        case e: Exception =>
          throw BrainfuckException(s"Error executing instruction '${instruction.at(decodePC)}'.", e)
      }
    }
    println()
  }
}
