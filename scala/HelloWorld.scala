sealed trait Symbol
case class Note(
    name: String, 
    duration: String, 
    octave: Int
) extends Symbol

case class Rest(
    duration: String
) extends Symbol

class Test {
    def sayHi(args: Array[String]) = {
        val Greets = args.map((x: String) => s"Hello, ${x}!").mkString("\n")
        println(Greets)
        println("Hello, World!")
    }
    def nonExhaustiveDuration(symbol: Symbol): String = symbol match {
        case Rest(duration) => duration
        case Note(name, duration, octave) => octave.toString
    }
}

object HelloWorld {
    def main(args: Array[String]): Unit = {
        val t = new Test()
        t.sayHi(args)
    }
}