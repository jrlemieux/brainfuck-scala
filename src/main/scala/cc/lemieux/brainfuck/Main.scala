package cc.lemieux.brainfuck

import scala.io.Source

object Main {

  // The throwable, the trowable cause and the throwable message can all be null,
  // handle these cases properly.
  def decodeThrowable(nullableThrowable: Throwable): String = {

    def decodeThrowableInner(nullableThrowable: Throwable): Seq[String] =
      Option(nullableThrowable).map { t =>
        (t.getClass().getName() + Option(t.getMessage()).map(": " + _).getOrElse("")) +: decodeThrowableInner(t.getCause())
      }.getOrElse(Seq.empty)

    decodeThrowableInner(nullableThrowable).mkString("; ")
  }

  private def mainInner(brainfuckProgramFileName: String, ramSize: Int, list: Boolean, debug: Boolean) = {
    val program = Compiler(brainfuckProgramFileName).compile()
    if (list)
      program.list()
    program.run(ramSize, debug)
  }

  def main(args: Array[String]): Unit =
    try
      if (args.length == 4)
        mainInner(args(0), args(1).toInt, args(2).toBoolean, args(3).toBoolean)
      else
        throw BrainfuckException("Invalid arguments.")
    catch {
      case t: Throwable =>
        System.err.println("\n\nFatal error: " + decodeThrowable(t))
    }
}
