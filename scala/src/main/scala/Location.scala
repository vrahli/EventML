object Location {
  class Location(ident: String, host: String, pn: Int) {
    def id   : String = ident
    def ip   : String = host
    def port : Int    = pn
/*
    var prev_list = List[Location]()
    var next_list = List[Location]()
 */

    override def toString : String = {
      "(loc:" + id + ",ip: " + ip + ",port:" + port.toString + ")"
    }
  }

  def main(args: Array[String]) { () }
}
