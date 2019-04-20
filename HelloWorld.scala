object HelloWorld {
    def main(args: Array[String]) {
        for(item <- args) {
            println(item)
        }
        println("Hello, World!")
    }
}