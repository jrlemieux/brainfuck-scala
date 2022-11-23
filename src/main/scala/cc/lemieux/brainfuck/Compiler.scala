package cc.lemieux.brainfuck

import cc.lemieux.brainfuck.CPU._

case class Compiler() {

  val stack = scala.collection.mutable.Stack[TestZeroAndJump]()
  var pc = 0

  def generate(instruction: Instruction) = {
    pc += 1;
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
          generate(IncrementPtr(+1))
        case '<' =>
          generate(IncrementPtr(-1))
        case '+' =>
          generate(IncrementValue(+1))
        case '-' =>
          generate(IncrementValue(-1))
        case '[' =>
          val testZeroAndJump = TestZeroAndJump(pc)
          stack.push(testZeroAndJump)
          generate(testZeroAndJump)
        case ']' =>
          val i = stack.pop()
          val loopStartPC = i.jumpPC
          i.jumpPC = pc + 1
          generate(Goto(loopStartPC))
        case ',' =>
          generate(ReadChar())
        case '.' =>
          generate(PrintChar())
        case _ =>
          IndexedSeq.empty
      }) ++ compileInner(program.tail)

  def compile(brainfuckProgramSource: String) = {
    val program = compileInner(brainfuckProgramSource)
    if (stack.length != 0)
      throw new BrainfuckException("Invalid program.")
    program
  }
}
