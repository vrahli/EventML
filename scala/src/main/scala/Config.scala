import scala.util.parsing.input.{ StreamReader, Reader }
import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.collection.mutable.HashMap
import java.io._
import scala.io._
import N2Sprelude._
import N2Sstrings._
import Location._


object Config extends RegexParsers {

  /*

   ----------------------------------------------------

   CLASSES

   -------------------------------------------------------

   */

  class Group(gname: String, list: List[Location], external: Boolean) {
    def name    : String         = gname
    def ext     : Boolean        = external
    def members : List[Location] = list

    /** Extracts the lists of lower and higher locations */
    def get_lt_and_gt(loc: Location) : (List[Location],List[Location]) = {
      val ip   = loc.ip
      val port = loc.port
      var lt : List[Location] = Nil
      var gt : List[Location] = Nil
      members.foreach { loc2 =>
        {
          val ip2   = loc2.ip
          val port2 = loc2.port
          if (ip < ip2) { gt ::= loc2 }
          else {
            if (ip == ip2) {
              if (port < port2) { gt ::= loc2 }
              else {
                if (port == port2) {}
                else {lt ::= loc2}
              }
            }
            else { lt ::= loc2 }
          }
        }
      }
      return (lt,gt)
    }

    override def toString : String = {
      var result = "group " + name + (if (ext) {" external "} else {""}) + "\n"
      members.foreach { m => result += "  " + m.toString + "\n" }
      return result
    }
  }

  class Groups(list: List[Group]) {
    def groups : HashMap[String,Group] =
      {
	var result = new HashMap[String,Group]()
	for (i <- 0 to list.length - 1) {
          result += list(i).name -> list(i)
	}
	return result
      }

    /** get location from name */
    def get_loc_from_loc(id: String) : Location = {
      var ret : Option[Location] = None
      groups.foreach { kv =>
        kv._2.members.foreach { loc => if (loc.id == id) { ret = Some(loc) } }
      }
      ret match {
        case None => throw new Exception("location not found: " + id)
        case Some(loc) => return loc
      }
    }

    /** Extracts groupname, ip, port number, and external */
    def get_info_from_loc(id: String) : (String,String,Int,Boolean) = {
      var ret : Option[(String,String,Int,Boolean)] = None
      groups.foreach { kv =>
        {
          val groupname = kv._1
          kv._2.members.foreach { loc =>
            if (loc.id == id) { ret = Some(groupname,loc.ip,loc.port,kv._2.ext) }
          }
        }
      }
      ret match {
        case None => throw new Exception("location not found: " + loc)
        case Some((gname,ip,port,ext)) => return(gname,ip,port,ext)
      }
    }

    /** get the lists of location */
    def get_locs () : List[Location] = {
      var ret : List[Location] = Nil
      groups.foreach { kv => ret ++= kv._2.members }
      return ret
    }

    /** get the lists of non-external location */
    def get_internal_locs () : List[Location] = {
      var ret : List[Location] = Nil
      groups.foreach { kv => if (!kv._2.ext) { ret ++= kv._2.members } }
      return ret
    }

    /** true if not external */
    def is_member(loc: String) : Boolean = {
      val (gname,ip,port,ext) = get_info_from_loc(loc)
      return ext
    }

    override def toString : String = {
      var result = ""
      groups.foreach { kv => result += kv._2.toString }
      return result
    }
  }

  class Connection(loc1: String, loc2: String) {
    def from : String = loc1
    def to   : String = loc2

    override def toString : String = "\t" + from + "\t=>\t" + to
  }

  class Connections(conns: List[Connection]) {
    def connections : List[Connection] = conns

    def get_lt_and_gt_groups (gname: String) : (List[String],List[String]) = {
      var lt : List[String] = Nil
      var gt : List[String] = Nil
      connections.foreach { conn =>
        {
          if (gname == conn.from) { gt ::= conn.to }
          if (gname == conn.to) { lt ::= conn.from }
        }
      }
      return (lt,gt)
    }

    override def toString : String = {
      var result = "Connections:" + "\n"
      connections.foreach { conn => result += "  " + conn.toString + "\n" }
      return result
    }
  }

  class Parameter(pkey: String, pval: Any, pstr : String) {
    def name : String = pkey
    def obj  : Any    = pval
    def str  : String = pstr

    override def toString : String = "\t" + name + "\t:\t" + obj.toString
  }

  class Parameters(lst: List[Parameter]) {
    def parameters : HashMap[String,Parameter] =
      {
	var result = new HashMap[String,Parameter]()
	for (i <- 0 to lst.length - 1) {
          result += lst(i).name -> lst(i)
	}
	return result
      }

    def list : List[Parameter] = lst

    override def toString : String = {
      var result = "Parameters:" + "\n"
      parameters.foreach { kv => result += kv._2.toString + "\n" }
      return result
    }
  }

  class Message (mkey: String, mval: String) {
    def destination : String = mkey
    def msg : String = mval

    override def toString : String = "\t" + destination + "\t:\t" + msg + "\n"
  }

  class Messages(list: List[Message]) {
    def messages : List[Message] = list

    override def toString : String = {
      var result = "Messages: " + "\n"
      messages.foreach { m => result += m.toString }
      result += "\n"
      return result
    }
  }

  class Conf(locations : Groups, connections : Connections, parameters : Parameters, messages : Messages) {
    def groups : Groups      = locations
    def conns  : Connections = connections
    def params : Parameters  = parameters
    def msgs   : Messages    = messages

    def get_loc_from_loc(id: String) : Location =
      groups.get_loc_from_loc(id)

    /** Extracts groupname, ip, port number, and external */
    def get_info_from_loc(loc: String) : (String,String,Int,Boolean) =
      groups.get_info_from_loc(loc)

    /** get the lists of location */
    def get_locs() : List[Location] = groups.get_locs()

    /** get the lists of non-external location */
    def get_internal_locs() : List[Location] =
      groups.get_internal_locs()

    def get_lt_and_gt_groups(gname: String) : (List[String],List[String]) =
      conns.get_lt_and_gt_groups(gname)

    /** Extracts the lists of lower and higher locations */
    def get_lt_and_gt(id: String) : (List[Location],List[Location]) = {
      val loc = groups.get_loc_from_loc(id)
      val (gname,ip,port,ext) = get_info_from_loc(id)
      var lt : List[Location] = Nil
      var gt : List[Location] = Nil
      val (lts,gts) = get_lt_and_gt_groups(gname)
      groups.groups.foreach { kv =>
        if (gname == kv._1) {
          val (lt2,gt2) = kv._2.get_lt_and_gt(loc)
          lt ++= lt2
          gt ++= gt2
        } else {
          if (lts.contains(kv._1)) { lt ++= kv._2.members }
          else { if (gts.contains(kv._1)) { gt ++= kv._2.members } }
        }
      }
      return (lt,gt)
    }

    /** true if not external */
    def is_member(loc: String) : Boolean =
      groups.is_member(loc)

    override def toString : String = {
      var result = "---- begin configuration file ----" + "\n"
      result += groups.toString
      result += conns.toString
      result += params.toString
      result += msgs.toString
      result += "---- end configuration file ----"
      return result
    }
  }


  /*

   ----------------------------------------------------

   PARSER

   -------------------------------------------------------

   */

  private def digit = "[0-9]".r
  private def letter = "[a-zA-Z]".r
  //regex(new Regex("[!_'%-]")) | regex(new Regex("\\\\[^{}()\\s:,;]")) |
  private def dot = "[.]".r
  private def symbol = "[ (){};,._~=`\\\\]".r |
    //32.toChar | 40.toChar | 41.toChar | 123.toChar | 125.toChar | 59.toChar | 46.toChar | 95.toChar | 92.toChar |
    42.toChar | 43.toChar | 47.toChar | 60.toChar | 63.toChar | 64.toChar | 124.toChar
  private def simpleSymbol = "[_'`]".r
  // control character /004   
  //private def endEsc = (new Regex(4.toChar + "") | "")
  private def escape = "[\\s]".r | 4.toChar
  private def escapes = (escape*)
  private def escapesp = (escape+)

  private def loc = "%locations"
  private def con = "%connections"
  private def par = "%parameters"
  private def msg = "%messages"
  private def dbs = "%databases"
  private def ext = "external"

  private def lb = "{"
  private def rb = "}"

  private def scol = ";"
  private def col  = ":"

  private def kwloc:  Parser[String] = "LOC\\b".r
  private def kwtype: Parser[String] = "TYPE\\b".r
  private def kwdeq:  Parser[String] = "DEQ\\b".r
  private def kwif:   Parser[String] = "if\\b".r
  private def kwthen: Parser[String] = "then\\b".r
  private def kwelse: Parser[String] = "else\\b".r

  private def reserved: Parser[String] = ( kwloc | kwtype | kwdeq | kwif | kwthen | kwelse )

  private def seq: Parser[String] =
    ((digit | letter | symbol)*) ^^ { s => s.mkString("").trim() }
  private def seq2: Parser[String] =
    ((digit | letter | simpleSymbol)*) ^^ { s => s.mkString("").trim() }
  private def ident: Parser[String] =
    (not(reserved) ~> (letter ~ seq)) ^^ { case l ~ s => l + s }
  private def ident2: Parser[String] =
    (not(reserved) ~> (letter ~ seq2)) ^^ { case l ~ s => l + s }
  private def num: Parser[Int] = (digit+) ^^ { case s => s.mkString("").toInt }

  private def groupname: Parser[String] = ident

  private def locid = ident
  private def locpn = num
  private def locip = (digit+) ~ "." ~ (digit+) ~ "." ~ (digit+) ~ "." ~ (digit+) ^^ {
    case d1 ~ p1 ~ d2 ~ p2 ~ d3 ~ p3 ~ d4 =>
      (d1.mkString("") + "." + d2.mkString("") + "." + d3.mkString("") + "." + d4.mkString("")).mkString("").trim()
  }

  private def location =
    escapes ~> locid ~ ((escapes ~ col ~ escapes) ~> locip) ~ (escapes ~> locpn) <~ escapes ^^ {
      case id ~ ip ~ pn => new Location(id, ip, pn)
    }
  private def locations = (location+) ^^ { case locs => locs.toList }

  private def group: Parser[Group] =
    ((escapes ~ loc) ~> (escapes ~> ext) ~ (escapes ~> groupname) ~ (escapes ~> locations) <~ escapes
      | (escapes ~ loc) ~> (escapes ~> groupname) ~ (escapes ~> locations) <~ escapes
    ) ^^ {
      case ext ~ (groupname : String) ~ loclist => {
        new Group(groupname, loclist, true)
      }
      case (groupname : String) ~ loclist => {
        new Group(groupname, loclist, false)
      }
    }

  private def groups: Parser[Groups] = (group+) ^^ { case groups => new Groups(groups) }

  private def connection: Parser[Connection] =
    escapes ~> groupname ~ ((escapes ~ "->" ~ escapes) ~> groupname) <~ escapes ^^ {
      case loc1 ~ loc2 => new Connection(loc1, loc2)
    }

  private def connections: Parser[List[Connection]] =
    (connection+) ^^ { case conns => conns.toList }

  private def conns: Parser[Connections] =
    ((escapes ~ con ~ escapes) ~> connections <~ escapes) ^^ {
      case conns => new Connections(conns)
    }

  private def paramkey = ident2

  private def atty: Parser[Unit] =
    (ident2) ^^ {
      case t => {
        t match {
	  case "Tok" => ()
	  case _ => throw new Exception("unknown atomic type: " + t)
        }
      }
    }

  private def tyc: Parser[Unit] =
    (atty ~ (escapes ~> ident2) | atty) ^^ {
      case t ~ s => {
        s match {
	  case "List" => ()
	  case _ => throw new Exception("unknown type constructor: " + s)
        }
      }
      case (t : String) => ()
    }

  private def atdeq: Parser[(Any,String)] =
    (ident2) ^^ {
      case t => {
        t match {
	  case "Tok" => { (nuprlTokDeq, "nuprlTokDeq") }
	  case _ => throw new Exception("unknown atomic type: " + t)
        }
      }
    }

  private def deqc: Parser[Any] =
    (atdeq ~ (escapes ~> ident2) | atdeq) ^^ {
      case (t : (Any,String)) ~ s => {
        s match {
	  case "List" => { (nuprlListDeq(t._1), "nuprlListDeq(" + t._2 + ")") }
	  case _ => throw new Exception("unknown type constructor: " + s)
        }
      }
      case (t : (Any,String)) => t
    }

  private def iteexp: Parser[(HashMap[String,Any] => Any, String)] =
    (((escapes ~ "if" ~ escapes) ~> expression) ~ ((escapes ~ "then" ~ escapes) ~> expression) ~ ((escapes ~ "else" ~ escapes) ~> expression) <~ escapes) ^^ {
      case exp1 ~ exp2 ~ exp3 =>
        {
	  ((lst : HashMap[String,Any]) =>
	    if (exp1._1(lst).asInstanceOf[Boolean]) { exp2._1(lst) }
	    else { exp3._1(lst) },
            "if ((" + exp1._2 + ").asInstanceOf[Boolean]) { " + exp2._2 + " }\n"
              + "else { " + exp3._2 + " }"
          )
	}
    }

  private def eqexp: Parser[(HashMap[String,Any] => Any, String)] =
    (escapes ~> atexp ~ ((escapes ~ "=" ~ escapes) ~> atexp) <~ escapes) ^^ {
      case exp1 ~ exp2 =>
	{
          ((lst : HashMap[String,Any]) =>
	    to_string(exp1._1(lst)) == to_string(exp2._1(lst)),
            "to_string(" + exp1._2 + ")" + " == " + "to_string(" + exp2._2 + ")")
        }
    }

  private def intexp: Parser[(HashMap[String,Any] => Any, String)] =
    (escapes ~> num <~ escapes) ^^ {
      case (i : Int) =>
	{ ((lst : HashMap[String,Any]) => i, i.toString) }
    }

  private def locexp: Parser[(HashMap[String,Any] => Any, String)] =
    ((escapes ~ "LOC(")  ~> ident2 <~ (")" ~ escapes)) ^^ {
      case (i : String) =>
	{ ((lst : HashMap[String,Any]) => i, "\"" + i + "\"") }
    }

  private def typexp: Parser[(HashMap[String,Any] => Any, String)] =
    ((escapes ~ "TYPE(") ~> tyc <~ (")" ~ escapes)) ^^ {
      case (i : Unit) =>
	{ ((lst : HashMap[String,Any]) => "TYPE", "\"" + "TYPE" + "\"") }
    }

  private def deqexp: Parser[(HashMap[String,Any] => Any, String)] =
    ((escapes ~ "DEQ(")  ~> deqc <~ (")" ~ escapes)) ^^ {
      case (i : (Any,String)) =>
	{ ((lst : HashMap[String,Any]) => i._1, i._2) }
    }

  private def explist : Parser[(HashMap[String,Any] => Any, String)] =
    (explistl | escapes) ^^ {
      case (exps : (HashMap[String,Any] => Any, String)) => exps
      case esc => { ((lst : HashMap[String,Any]) => Ax, "Ax") }
    }

  private def explistl : Parser[(HashMap[String,Any] => Any, String)] =
    (escapes ~> expression ~ ((escapes ~ scol ~ escapes) ~> explistl) <~ escapes
     | escapes ~> expression <~ escapes) ^^ {
      case (exp : (HashMap[String,Any] => Any, String)) ~ (exps : (HashMap[String,Any] => Any, String)) =>
	{
          ((lst : HashMap[String,Any]) => ( exp._1(lst) , exps._1(lst) ),
            mk_pair(exp._2,exps._2))
        }
      case (exp : (HashMap[String,Any] => Any, String)) =>
	{
          ((lst : HashMap[String,Any]) => ( exp._1(lst) , Ax ),
            mk_pair(exp._2,"Ax"))
        }
    }

  private def listexp: Parser[(HashMap[String,Any] => Any, String)] =
    ((escapes ~ lb) ~> explist <~ (rb ~ escapes)) ^^ {
      case explist =>
	{ ((lst : HashMap[String,Any]) => explist._1(lst), explist._2) }
    }

  private def idexp: Parser[(HashMap[String,Any] => Any, String)] =
    (escapes ~> ident2 <~ escapes) ^^ {
      case (id : String) =>
	{
	  ((lst : HashMap[String,Any]) =>
	    if (lst.contains(id)) { nuprl_v(lst(id)) }
	    else { throw new Exception("unbound id: " + id) },
            mk_nuprl_v(id))
	}
    }

  private def lamexp: Parser[(HashMap[String,Any] => Any, String)] =
    ((escapes ~ "\\") ~> (ident2 <~ dot) ~ expression <~ escapes) ^^ {
      case (id : String) ~ exp =>
	{
          ((lst : HashMap[String,Any]) => (x : Any) => exp._1(lst+= id -> x),
            mk_fun1(id,exp._2))
        }
    }

  private def pexp: Parser[(HashMap[String,Any] => Any, String)] =
    ("(" ~> expression <~ ")") ^^ {
      case (exp : (HashMap[String,Any] => Any, String)) =>
	{ (exp._1, "(" + exp._2 + ")") }
    }

  private def atexp: Parser[(HashMap[String,Any] => Any, String)] =
    ( intexp | locexp | typexp | deqexp | listexp | idexp | pexp )

  private def expression: Parser[(HashMap[String,Any] => Any, String)] =
    ( iteexp | eqexp | lamexp | atexp )

  private def parameter =
    (escapes ~> paramkey ~ ((escapes ~ col ~ escapes) ~> expression) <~ escapes) ^^ {
      case pk ~ exp =>
	new Parameter(pk, exp._1(new HashMap[String,Any]()), exp._2)
    }

  private def parameters = (parameter*) ^^ { case pars => pars.toList }

  private def params: Parser[Parameters] =
    ((escapes ~ par ~ escapes) ~> parameters <~ escapes) ^^ {
      case prms => new Parameters(prms)
    }

  private def messagekey = ident
  private def messageval = seq
  private def message =
    (escapes ~ messagekey ~ escapes ~ col ~ escapes ~ messageval ~ escapes) ^^ {
      case esc1 ~ mk ~ esc2 ~ col ~ esc3 ~ mv ~ esc4 =>
	new Message(mk, mv)
    }
  private def messages = (message*) ^^ { case msgs => msgs.toList }

  private def msgs: Parser[Messages] =
    ((escapes ~ msg ~ escapes) ~> messages <~ escapes) ^^ {
      case msgs => new Messages(msgs)
    }

  private def dbkey = seq
  private def dbval = seq
  private def db =
    (escapes ~ dbkey ~ escapes ~ col ~ escapes ~ dbval ~ escapes) ^^ {
      case esc1 ~ mk ~ esc2 ~ col ~ esc3 ~ mv ~ esc4 => ()
    }
  private def dblist = (db*) ^^ { case db => db.toList }

  private def databases: Parser[Unit] =
    (escapes ~ dbs ~ escapes ~ dblist ~ escapes) ^^ {
      case esc1 ~ dbs ~ esc2 ~ dblist ~ esc3 => ()
    }

  private def terms: Parser[Conf] =
    ((escapes ~> groups)
     ~ (escapes ~> conns)
     ~ (escapes ~> params)
     ~ (escapes ~> msgs)
     ~ (escapes ~> databases) <~ escapes
    ) ^^ {
      case groups ~ conns ~ params ~ msgs ~ dbs =>
        new Conf(groups,conns,params,msgs)
    }

  override val skipWhitespace = false // meaningful spaces in our regex

  def getTerms(): Parser[Conf] = { return terms; }

    /*

     1st argument: configuration file

     */
  def main(args: Array[String]) {

    val curTime   = System.currentTimeMillis
    val conf_file = args(0)

    println(" + parsing config file: " + conf_file)
    val input  = StreamReader(new InputStreamReader(new FileInputStream(conf_file)))
    val output = parseAll(terms, input)
    val conf   = output.get
    println(" - done")

    println(conf.toString)
    println("ldrs_uid: " + conf.params.parameters("ldrs_uid").obj.asInstanceOf[Any => Any])
    println("ldrs_uid: " + nuprl_apply(conf.params.parameters("ldrs_uid").obj,"ldr2"))

    // println("making parameter list")

    // // making parameter list
    // var paramList = List[String]()
    // for (i <- 0 to list_class.length - 1) {
    //   if (list_class(i).isInstanceOf[Locations]) {
    //     //printList(list_class(i).asInstanceOf[Locations])
    //   }
    //   if (list_class(i).isInstanceOf[Parameters]) {
    //     var pList = list_class(i).asInstanceOf[Parameters].paramter_list
    //     for (i <- 0 to pList.length - 1) {
    //       paramList ::= pList(i).asInstanceOf[Parameter].parameter_val.toString
    //     }
    //   }

    // }

    // paramList = paramList.reverse
    // println(paramList)


    //val writer = new PrintWriter(new File("result"))
    //writer.write(result.toString())
    //writer.close()

    // println("making process")

    // N2S.mainprocess(args(0), paramList, args(2))

    // println("process made!")

  }
}
