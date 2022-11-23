package cc.lemieux.brainfuck

object CPU {

  trait Instruction {
    def at(index: Int) = f"$index%04d: $this"
  }

  case class UpdatePtr(by: Int) extends Instruction

  case class UpdateValue(by: Int) extends Instruction

  // var because there is a fixup when the ] is found.
  case class TestZeroAndJump(var jumpPC: Int) extends Instruction

  case class Goto(jumpPC: Int) extends Instruction

  case class ReadChar() extends Instruction

  case class PrintChar() extends Instruction

  case class Halt() extends Instruction
}

import CPU._

case class CPU(ramSize: Int, debug: Boolean) {

  // Short are used because a Byte is a signed integer under JVM.
  // Using Short allows us to have positive values 0 to 255.

  type BrainfuckByte = Short

  def toBrainfuckByte(i: Int) = (i & 0xFF).toShort

  private val ram = new Array[BrainfuckByte](ramSize)

  private var pc: Int = _
  private var ptr: Int = _
  private var halted: Boolean = _
  private var someOutput: Boolean = _

  // Put printed characters between <> when debugging.
  val (printStart, printEnd) = if (debug) ("<", ">") else ("", "")

  private def reset() = {
    pc = 0
    ptr = 0
    halted = false
    someOutput = false
  }

  def makeDebugInfo(s: => String) = if (debug) Some(s) else None

  // Execute the instruction, and return an optional debugging string.
  private def decode(instruction: Instruction): Option[String] =
    instruction match {
      case UpdatePtr(by) =>
        ptr += by
        pc += 1
        makeDebugInfo(s"pointer = $ptr")
      case UpdateValue(by) =>
        ram(ptr) = toBrainfuckByte(ram(ptr) + by)
        pc += 1
        makeDebugInfo(s"ram($ptr) = ${ram(ptr)}")
      case TestZeroAndJump(jumpPC) =>
        val (newPC, debugInfo) = if (ram(ptr) == 0) (jumpPC, makeDebugInfo(s"tested as 0, jumping to $jumpPC")) else (pc + 1, None)
        pc = newPC
        debugInfo
      case Goto(jumpPC) =>
        pc = jumpPC
        makeDebugInfo(s"jumping to $jumpPC")
      case ReadChar() =>
        ram(ptr) = toBrainfuckByte(System.in.read())
        pc += 1
        makeDebugInfo(s"ram($ptr) read as ${ram(ptr)}")
      case PrintChar() =>
        System.out.print(s"$printStart${ram(ptr).toChar}$printEnd")
        someOutput = true
        pc += 1
        None
      case Halt() =>
        halted = true
        makeDebugInfo("terminated")
    }

  def run(program: BrainfuckProgram) = {
    reset()
    while (!halted) {
      val decodePC = pc
      val instruction = program.instructions(decodePC)
      if (debug)
        System.err.print(instruction.at(decodePC))
      try {
        decode(instruction).foreach(debugInfo => System.err.print("; " + debugInfo))
        if (debug)
          System.err.println()
      } catch {
        case t: Throwable =>
          val moreInfo = t match {
            case _: ArrayIndexOutOfBoundsException => s"; pointer reached $ptr while the valid range is 0 to ${ramSize - 1}"
            case _ => ""
          }
          throw BrainfuckException(s"Can't execute instruction ${instruction.at(decodePC)}" + moreInfo + ".", t)
      }
    }
    if (someOutput)
      println()
  }
}
