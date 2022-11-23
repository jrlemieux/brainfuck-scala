package cc.lemieux.brainfuck

case class BrainfuckException(why: String, e: Exception = null) extends RuntimeException(why, e)

