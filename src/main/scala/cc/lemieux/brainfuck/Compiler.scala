package cc.lemieux.brainfuck

import cc.lemieux.brainfuck.CPU._

import scala.io.Source

case class Compiler(brainfuckProgramFileName: String) {

  val brainfuckProgramSource = Source.fromFile(brainfuckProgramFileName).getLines().mkString("\n")

  private val stack = scala.collection.mutable.Stack[TestZeroAndJump]()
  private var pc: Int = _

  private def reset() = {
    stack.clear()
    pc = 0
  }

  private def generate(instruction: Instruction) = {
    pc += 1
    IndexedSeq(instruction)
  }

  private def compileInner(program: String): IndexedSeq[Instruction] =
    if (program == "")
      generate(Halt())
    else if (program.head == '#')
      program.indexOf('\n') match {
        case -1 => IndexedSeq.empty
        case pos => compileInner(program.drop(pos + 1))
      }
    else
      (program.head match {
        case '>' =>
          generate(UpdatePtr(+1))
        case '<' =>
          generate(UpdatePtr(-1))
        case '+' =>
          generate(UpdateValue(+1))
        case '-' =>
          generate(UpdateValue(-1))
        case '[' =>
          val testZeroAndJump = TestZeroAndJump(pc)
          stack.push(testZeroAndJump)
          generate(testZeroAndJump)
        case ']' =>
          val i = stack.pop()
          val loopStartPC = i.jumpPC
          // Fixup.
          i.jumpPC = pc + 1
          generate(Goto(loopStartPC))
        case ',' =>
          generate(ReadChar())
        case '.' =>
          generate(PrintChar())
        case _ =>
          IndexedSeq.empty
      }) ++ compileInner(program.tail)

  def compile(): BrainfuckProgram = {
    reset()
    val instructions = compileInner(brainfuckProgramSource)
    if (stack.length != 0)
      throw new BrainfuckException("Invalid Brainfuck program.")
    BrainfuckProgram(instructions)
  }
}
