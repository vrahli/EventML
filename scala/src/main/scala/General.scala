object General {

  def printEmlLoc(loc: String, str: => String) = {
    println("  " + loc + ": " + str)
  }

  def printEmlLoc(loc: String, sub: => String, str: => String) = {
    println("  " + loc + "-" + sub + ": " + str)
  }

  def printEmlLocDbg(loc: String, sub: => String, str: => String) = {
    printEmlLoc(loc,sub,str)
  }

  def main(args: Array[String]) = { () }

}
