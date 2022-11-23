package cc.lemieux.brainfuck

case class BrainfuckException(why: String, t: Throwable = null) extends RuntimeException(why, t)

