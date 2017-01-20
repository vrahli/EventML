import scala.collection.mutable.HashMap
import N2Sstrings._
import N2Sprelude._

/**
 * N2Sclass
 * It contains all the N2S case classes till now.
 * primitive + non-primitive
 */
object N2Sclass {

  // 1st string is the name of the function and
  // 2nd string is the definition of the function
  //
  // for example if we end up using btrue in a program, we're going
  // to add ("btrue","Inl(it)") to `functions`
  // and because it is not a primitive, we're also going to have
  // ("it","Axiom") in `functions`
  // functions = [ ... , ("it","Axiom") , ..., ("btrue","Inl(it)") , ... ]
  //
  var functions = new HashMap[String,String]()
  var lib = new HashMap[String,(TreeNode,TreeNode)]()
  var missing_opid: List[String] = Nil

  class N2Sparam() {
    private var lazyvars  : Set[String] = Set.empty
    private var eagervars : Set[String] = Set.empty
    private var recs = new HashMap[String,(Int,List[TreeNode],List[TreeNode])]()

    def setLazyVars(vs: Set[String]) : this.type = { lazyvars = vs; return this }
    def setEagerVars(vs: Set[String]) : this.type = { eagervars = vs; return this }
    def setRecs(rs: HashMap[String,(Int,List[TreeNode],List[TreeNode])]) : this.type = { recs = rs; return this }

    def copy(): N2Sparam = (new N2Sparam()).setLazyVars(lazyvars).setEagerVars(eagervars).setRecs(recs)

    def addLazyVar(v: String): this.type = {
      lazyvars += v
      recs -= v
      return this
    }
    def addEagerVar(v: String): this.type = {
      eagervars += v
      recs -= v
      return this
    }

    def addLazyVars(vs: List[String]): this.type = {
      lazyvars ++= vs
      vs.foreach(v => recs -= v)
      return this
    }
    def addEagerVars(vs: List[String]): this.type = {
      eagervars ++= vs
      vs.foreach(v => recs -= v)
      return this
    }

    def rmVar(v: String): this.type = {
      lazyvars  -= v
      eagervars -= v
      return this
    }

    def isLazyVar(v: String): Boolean = lazyvars.contains(v)
    def isEagerVar(v: String): Boolean = eagervars.contains(v)
    def isRec(v: String): Boolean = recs.contains(v)

    def getRec(v: String): (Int,List[TreeNode],List[TreeNode]) = recs(v)

    def addRec(opid:String, n:Int, paramsFront:List[TreeNode], paramsBack:List[TreeNode])
        : this.type = {
      recs += opid -> (n,paramsFront,paramsBack)
      return this
    }

    override def toString() : String = {
      "lazyvars: " + lazyvars + "\n" +
      "eagervars: " + eagervars
    }
  }

  def toVars(ts: List[TreeNode]) : List[TreeNode] = {
    ts.map { t =>
      if (t.topid == "bound_id") { new VariableNode(t.tsubterms(0).tparams(0)) }
      else { t }
    }
  }

  def isRecursiveFunction(opid: String, params: List[TreeNode], rhs: TreeNode) = {
    rhs.destApplies() match {
      case head :: args => {
        if (head.isFix() // application of a fix to arguments
          && args.forall(t => t.isVar()) // all the arguments have to be variables
          && params.length >= args.length // we have at least as many parameters as we have arguments
        ) {
          val (paramsFront,paramsArgs1) = params.splitAt(params.length - args.length)
          val (paramsArgs2,paramsBack) = params.splitAt(args.length)
	  var pFront: List[TreeNode] = Nil
	  var pBack: List[TreeNode] = Nil
	  val b: Boolean = {
	    if (paramsArgs1.forall(t => t.isVar())
		&& args.zip(paramsArgs1).forall(p => p match { case (a,b) => a.getParam().pvalue == b.getParam().pvalue })
              ) { pFront = paramsFront; true }
	    else { if (paramsArgs2.forall(t => t.isVar())
		       && args.zip(paramsArgs2).forall(p => p match { case (a,b) => a.getParam().pvalue == b.getParam().pvalue })
		     ) { pBack = paramsBack; true }
		   else { false }
		}
	  }
          if (b) {
            val lam = head.tsubterms(0)
            if (lam.isLambda()) { // the subterm of fix should be a series of lambdas
              lam.destLambdas() match {
                case Some((h::otherparams,t)) => {
                  // otherparams is a list of parameters
		  if (h.pvalue == opid  // the variable of the 1st lambda should be opid
                    && otherparams.length == args.length // we should have as many lambdas as we have arguments
		    && otherparams.zip(args.map(t => t.getParam())).forall(p => p match { case (a,b) => a.pvalue == b.pvalue }) // the binders should have the same name as the arguments
                    && t.correctRecursiveCalls(opid,args.length,toVars(pFront ++ pBack).map{ t => t.getParam().pvalue }) // all the recursive calls to opid are applied to args.length arguments
                  ) {
                    // Now we have to return params1, and we have to make sure
                    // that all the applications of opid in t are turned to, first,
                    // applications to params1.
                    // Also, we have to change the applications into function calls.
                    Some((t,args.length,toVars(pFront),toVars(pBack)))
		  } else { None }
	        } // end of case Some
                case _ => { None }
              } // end of cases
            } else { None }
          } else { None }
        } else { None }
      }
      case _ => { None }
    }
  }

  class NuprlParameter(val pvalue: String, val ptype: String) {
    override def toString: String = "parameter:{" + pvalue + "," + ptype + "}"
    def toScala(n2sp: N2Sparam): String = {
      if (pvalue == "") { "_" }
      else {
	var v = replace_str(pvalue)
	if (n2sp.isLazyVar(pvalue)) { mk_nuprl_lazy_v(v) } else { v }
      } //+ ": " + typ
    }
  }

  abstract class TreeNode(opid: String, tag: String, params: List[NuprlParameter], subterms: List[TreeNode])
  {
    def topid = opid
    def tparams = params
    def tsubterms = subterms

    def occursAtFunctionPosition(v: String) : Boolean = {
      subterms.exists {subterm => subterm.occursAtFunctionPosition(v)}
    }

    def correctRecursiveCalls(opid:String,nargs:Int,prms:List[String]): Boolean = {
      subterms.forall{ t => t.correctRecursiveCalls(opid,nargs,prms) }
    }

    def isLambda() : Boolean = false
    def isApply()  : Boolean = false
    def isFix()    : Boolean = false
    def isVar()    : Boolean = false
    def getParam() : NuprlParameter = throw new Exception("getParam")

    def destApplies() : List[TreeNode] = List(this)
    def destLambdas() : Option[(List[NuprlParameter],TreeNode)] = None

    override def toString: String = {
      // val params_to_scala =
      //   (for (i <- 0 to params.length - 1) yield (params(i).toScala + ": Any")).mkString(", ")
      "TreeNode(opid:" + opid + ", tag:" + tag + ", params:{" + params.toString + "}, subterms:{" + subterms.toString + "})\n"
    }

    def toScala(n2sp: N2Sparam): String = throw new Exception("toScala of a TreeNode")
  }

  def lookup(id: String): (TreeNode, TreeNode) = {
    // definition could be two separated cases
    if (lib.contains(id) || lib.contains(id + "\\ def")) {
      return lib.get(id).get
    } else {
      return null
    } // TODO
  }

  // non-primitives
  case class TermNode(opid: String, tag: String, params: List[NuprlParameter], subterms: List[TreeNode])
      extends TreeNode(opid, tag, params, subterms) {
    // lookup the definition of opid in the alldefs file (in the AST)
    // length(term) ---> "length(" + term.toScala + ")"

    override def toScala(n2sp: N2Sparam) : String = {
      //println("converting " + opid + ", " + "looking up its definition in the library")
      lookup(opid) match {
        case (lhs, rhs) =>
          {
            //println("converting " + opid + ", " + "found definition")
            // 1st, check whether opid is already in `functions`
            if (!functions.contains(opid)) { // && (tag == "t" || tag.substring(0, 2) == "t,")) {
              //println("converting " + opid + ", " + "converting defintion to scala")
              // lhs.topid + "(" + (lhs.tsubterms.map(x => x.tparams(0).toScala + " : Any")).mkString(", ") + ")"
              // TO FIX (above)
              // something a bit more complicated to handle definitions like list_ind
              // not just x.tparams(0).toScala
              val fheader  = replace_str(lhs.topid)
	      val params   = lhs.tsubterms
              val vparams  = toVars(params)
              val sparams  = vparams.map{v => mk_one_lazy_param(v.tparams(0).toScala(new N2Sparam()))}.mkString("(",",",")")
              var rhsscala = ""
              isRecursiveFunction(opid,params,rhs) match {
                case None => { rhsscala = rhs.toScala(new N2Sparam()) }
                case Some((body,n,pFront,pBack)) => {
		  println("found recursive function: " + opid)
		  rhsscala = body.toScala((new N2Sparam()).addRec(opid,n,pFront,pBack))
		}
              }
              val func = "def " + fheader + sparams + " : Any = " + rhsscala + "\n"
              functions += opid -> func
            }
            //println("converting " + opid + ", " + "converting expression")
            //println("\n")
            return (replace_str(opid) + "(" + (subterms.map(x => x.toScala(n2sp))).mkString(", ") + ")")
          }
        case _ =>
          {
            println("converting " + opid + ", " + "lookup failed")
            if (!missing_opid.contains(opid)) { missing_opid ::= opid }
            //println("\n")
            //throw new Exception("lookup failed")
            return ""
          }
      }
    }
  }

  // primitives
  case class BoundIdNode(params: List[NuprlParameter], term: TreeNode) extends TreeNode("bound_id", "", params, term :: Nil) {
    override def toString: String = "BoundIdNode(" + params.toString + "," + term.toString + ")"
    // override def toScala: String = ""

    // boundids should be converted to functions
    // {bound_id,v,w,z}(term) -> (v) => (w) => (z) => term

    override def correctRecursiveCalls(opid:String,nargs:Int,prms:List[String]): Boolean = {
      params.forall{ p => !prms.contains(p.pvalue) && !(p.pvalue == opid) } &&
      term.correctRecursiveCalls(opid,nargs,prms)
    }

    override def toScala(n2sp : N2Sparam): String = {
      var bound_func = ""
      params.foreach {
	param =>
          bound_func += mk_param_lazy(param.toScala(new N2Sparam())) + " =>\n "
      }
      bound_func += "{" + term.toScala(n2sp.copy().addLazyVars(params.map(x => x.pvalue))) + "}"
      return bound_func
    }
  }

  // lambda, term, none
  case class LambdaNode(param: NuprlParameter, term: TreeNode) extends TreeNode("lambda", "", Nil, BoundIdNode(param :: Nil, term) :: Nil) {
    override def toString: String = {
      "LambdaNode(parameter:{" + param.toString + "} ,subterms:{" + term.toString + "})\n"
    }
    override def isLambda() : Boolean = true
    override def destLambdas() : Option[(List[NuprlParameter],TreeNode)] = {
      term.destLambdas() match {
        case Some((vs,t)) => Some((param::vs,t))
        case None => Some((List(param),term))
      }
    }
    override def toScala(n2sp : N2Sparam): String = {
      mk_fun1_lazy_n(
        param.toScala(new N2Sparam()),
        term.toScala(n2sp.copy().addLazyVar(param.pvalue))
      )
    }
    // we need to generate
    //      "(" + param.toScala + " : Any) => " + term.toScala
    // where all the occurrences of param in term have to be transformed into:
    //      cbv_scala_apply(param,"0")
  }

  // apply, lambda, pair, inl, inr
  case class ApplyNode(term1: TreeNode, term2: TreeNode) extends TreeNode("apply", "", Nil, term1 :: term2 :: Nil) {
    override def occursAtFunctionPosition(v: String) : Boolean = {
      (
	term1 match {
	  case VariableNode(param) => param.pvalue == v
	  case _ => term1.occursAtFunctionPosition(v)
	}
      ) || term2.occursAtFunctionPosition(v)
    }

    override def isApply() : Boolean = true

    override def destApplies() : List[TreeNode] = term1.destApplies() :+ term2

    override def correctRecursiveCalls(opid:String,nargs:Int,prms:List[String]): Boolean = {
      this.destApplies() match {
        case head :: tail => {
          if (head.isVar() && head.getParam().pvalue == opid) {
            (nargs == tail.length) &&
            (tail.forall(t => t.correctRecursiveCalls(opid,nargs,prms)))
          } else {
            term1.correctRecursiveCalls(opid,nargs,prms) &&
            term2.correctRecursiveCalls(opid,nargs,prms)
          }
        }
        case _ => throw new Exception("correctRecursiveCalls:destApplies")
      }
    }

    override def toString: String = {
      "ApplyNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    }

    override def toScala(n2sp: N2Sparam): String = {
      this.destApplies() match {
        case head :: tail => {
          if (head.isVar() && n2sp.isRec(head.getParam().pvalue)) {
            val id = head.getParam().pvalue;
            (n2sp.getRec(id)) match {
              case (n,paramsFront,paramsBack) => {
                if (n == tail.length) {
                  replace_str(id) +
                  "(" +
                    ((paramsFront ++ tail ++ paramsBack).map(x => x.toScala(n2sp))).mkString(", ") +
                  ")"
                } else { throw new Exception("application:toScala:rec:wrong_arg_length") }
              }
            }
          } else {
            mk_nuprl_apply(term1.toScala(n2sp), term2.toScala(n2sp))
          }
        }
        case _ => mk_nuprl_apply(term1.toScala(n2sp), term2.toScala(n2sp))
      }
    }
  }

  case class VariableNode(param: NuprlParameter) extends TreeNode("variable", "", param :: Nil, Nil) {
    override def isVar() : Boolean = true
    override def getParam() : NuprlParameter = param
    override def toString: String = "VariableNode(" + param.toString + ")"
    override def toScala(n2sp: N2Sparam): String = param.toScala(n2sp) //toParam
  }
  case class VarExpNode(param: NuprlParameter, subterms: List[TreeNode]) extends TreeNode("varexp", "", param :: Nil, subterms) {
    override def toString: String = {
      "VarExpNode(" + param.toString +
      "(" +
      (for (i <- 0 to subterms.length - 1) yield (subterms(i).toString)).mkString(", ") +
      "))"
    }
    // override def toScala: String = param.toScala +
    // "(" + (for(i <- 0 to subterms.length-1) yield(subterms(i).toScala)).mkString(", ") + ")" //toParam

    // Variables can have subterms.  We should treat that as applications.
    // v(t1,t2,t2) -> ((v.asInstanceOf[Any => Any](t1)).asInstanceOf[Any => Any](t2)).asInstanceOf[Any => Any](t3)
    // This is dealt with by VarExpNode
    override def toScala(n2sp: N2Sparam): String = {
      var varExp = param.toScala(n2sp)
      subterms.foreach {
	subterm => varExp = "(" + mk_nuprl_apply(varExp,subterm.toScala(n2sp)) + ")"
      }
      return varExp
    }
  }

  // minus, term, none, none
  case class MinusNode(term: TreeNode) extends TreeNode("minus", "", Nil, term :: Nil) {
    override def toString: String = "MinusNode(subterms:{" + term.toString + "})\n"
  }

  // add, term1, term2, none
  case class AddNode(term1: TreeNode, term2: TreeNode) extends TreeNode("add", "", Nil, term1 :: term2 :: Nil) {
    override def toString: String = {
      "AddNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      "add(" + term1.toScala(n2sp) + ", " + term2.toScala(n2sp) + ")"
    }
  }

  // subtract, term1, term2, none
  case class SubtractNode(term1: TreeNode, term2: TreeNode) extends TreeNode("subtract", "", Nil, term1 :: term2 :: Nil) {
    override def toString: String = {
      "SubtractNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      "sub(" + term1.toScala(n2sp) + ", " + term2.toScala(n2sp) + ")"
    }
  }

  // multiply, term1, term2, none
  case class MultiplyNode(term1: TreeNode, term2: TreeNode) extends TreeNode("multiply", "", Nil, term1 :: term2 :: Nil) {
    override def toString: String = {
      "MultiplyNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      "mul(" + term1.toScala(n2sp) + ", " + term2.toScala(n2sp) + ")"
    }
  }

  // divide, term1, term2, none
  case class DivideNode(term1: TreeNode, term2: TreeNode) extends TreeNode("divide", "", Nil, term1 :: term2 :: Nil) {
    override def toString: String = {
      "DivideNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      "div(" + term1.toScala(n2sp) + ", " + term2.toScala(n2sp) + ")"
    }
  }

  // remainder, term1, term2, none
  case class RemainderNode(term1: TreeNode, term2: TreeNode) extends TreeNode("remainder", "", Nil, term1 :: term2 :: Nil) {
    override def toString: String = {
      "RemainderNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      "rem(" + term1.toScala(n2sp) + ", " + term2.toScala(n2sp) + ")"
    }
  }

  // natural_number, param, none, none
  case class Natural_numberNode(param: NuprlParameter) extends TreeNode("natural_number", "", param :: Nil, Nil) {
    override def toString: String = "Natural_numberNode(params:{" + param.toString + "})\n"
    override def toScala(n2sp:N2Sparam): String = param.pvalue
  }

  // pair, term1, term2, none
  case class PairNode(term1: TreeNode, term2: TreeNode) extends TreeNode("pair", "", Nil, term1 :: term2 :: Nil) {
    override def toString: String = "PairNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = {
      mk_pair(term1.toScala(n2sp),term2.toScala(n2sp))
    }
  }

  // ispair, term1, term2, term3
  case class IspairNode(term1: TreeNode, term2: TreeNode, term3: TreeNode) extends TreeNode("ispair", "", Nil, term1 :: term2 :: term3 :: Nil) {
    override def toString: String = {
      "IspairNode(subterms:{" + term1.toString + ", " +
      term2.toString + ", " + term3.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      mk_ispair(term1.toScala(n2sp),term2.toScala(n2sp),term3.toScala(n2sp))
    }
  }

  // inl, term, none
  case class InlNode(term: TreeNode) extends TreeNode("inl", "", Nil, term :: Nil) {
    override def toString: String = {
      "InlNode(subterms:{" + term.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      term match {
        case TermNode("it",_,Nil,Nil) => "nuprlBTrue"
        case TermNode("nil",_,Nil,Nil) => "nuprlBTrue"
        case AxiomNode() => "nuprlBTrue"
        case _ => "new Inl(" + term.toScala(n2sp) + ")"
      }
    }
  }

  // isinl, term1, term2, term3
  case class IsinlNode(term1: TreeNode, term2: TreeNode, term3: TreeNode) extends TreeNode("isinl", "", Nil, term1 :: term2 :: term3 :: Nil) {
    override def toString: String = {
      "IsinlNode(subterms:{" + term1.toString + ", " +
      term2.toString + ", " + term3.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      mk_isinl(term1.toScala(n2sp),term2.toScala(n2sp),term3.toScala(n2sp))
    }
  }

  // inr, term, none
  case class InrNode(term: TreeNode) extends TreeNode("inr", "", Nil, term :: Nil) {
    override def toString: String = "InrNode(subterms:{" + term.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = {
      term match {
        case TermNode("it",_,Nil,Nil) => "nuprlBFalse"
        case TermNode("nil",_,Nil,Nil) => "nuprlBFalse"
        case AxiomNode() => "nuprlBFalse"
        case _ => "new Inr(" + term.toScala(n2sp) + ")"
      }
    }
  }

  // isinr, term1, term2, term3
  case class IsinrNode(term1: TreeNode, term2: TreeNode, term3: TreeNode) extends TreeNode("isinr", "", Nil, term1 :: term2 :: term3 :: Nil) {
    override def toString: String = {
      "IsinrNode(subterms:{" + term1.toString + ", " +
      term2.toString + ", " + term3.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      mk_isinr(term1.toScala(n2sp),term2.toScala(n2sp),term3.toScala(n2sp))
    }
  }

  // spread, term1, term2, none
  // case class SpreadNode(pair: TreeNode, param1 : NuprlParameter, param2 : NuprlParameter, term2: TreeNode)
  case class SpreadNode(term1: TreeNode, param1: NuprlParameter, param2: NuprlParameter, t: TreeNode)
    extends TreeNode("spread", "", Nil, term1 :: (BoundIdNode(param1 :: param2 :: Nil, t)) :: Nil) {
    override def toString: String = {
      "SpreadNode(subterms:{" + term1.toString + ", " + param1.toString + "." + param2.toString + "." + t.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      val p  = term1.toScala(n2sp)
      val p1 = param1.toScala(new N2Sparam())
      val p2 = param2.toScala(new N2Sparam())
      val b  = t.toScala(n2sp.copy().rmVar(param1.pvalue).rmVar(param2.pvalue))
      return mk_spread(p,p1,p2,b)
    }
  }

  // decide, term1, inl, inr
  case class DecideNode(term1: TreeNode, param1: NuprlParameter, t1: TreeNode, param2: NuprlParameter, t2: TreeNode)
    extends TreeNode("decide", "", Nil, term1 :: (BoundIdNode(param1 :: Nil, t1)) :: (BoundIdNode(param2 :: Nil, t2)) :: Nil) {
    override def toString: String = {
      "DecideNode(subterms:{" + term1.toString + ", " + param1.toString + "." + t1.toString + ", " + param2.toString + "." + t2.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      val d  = term1.toScala(n2sp)
      val p1 = param1.toScala(new N2Sparam())
      val b1 = t1.toScala(n2sp.copy().rmVar(param1.pvalue))
      val p2 = param2.toScala(new N2Sparam())
      val b2 = t2.toScala(n2sp.copy().rmVar(param2.pvalue))
      return mk_decide(d,p1,b1,p2,b2)
    }
  }

  // int_eq, term1, term2, term3, term4
  case class Int_eqNode(term1: TreeNode, term2: TreeNode, term3: TreeNode, term4: TreeNode) extends TreeNode("int_eq", "", Nil, term1 :: term2 :: term3 :: term4 :: Nil) {
    override def toString: String = {
      "Int_eqNode(subterms:{" +
      term1.toString + ", " +
      term2.toString + ", " +
      term3.toString + ", " +
      term4.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam) : String = {
      mk_int_eq(
        term1.toScala(n2sp),
        term2.toScala(n2sp),
        term3.toScala(n2sp),
        term4.toScala(n2sp))
    }
  }

  // less, term1, term2, term3, term4
  case class LessNode(term1: TreeNode, term2: TreeNode, term3: TreeNode, term4: TreeNode) extends TreeNode("less", "", Nil, term1 :: term2 :: term3 :: term4 :: Nil) {
    override def toString: String = {
      "LessNode(subterms:{" +
      term1.toString + ", " +
      term2.toString + ", " +
      term3.toString + ", " +
      term4.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      mk_less(
        term1.toScala(n2sp),
        term2.toScala(n2sp),
        term3.toScala(n2sp),
        term4.toScala(n2sp))
    }
  }

  // isint, term1, term2, term3
  case class IsintNode(term1: TreeNode, term2: TreeNode, term3: TreeNode) extends TreeNode("isint", "", Nil,
    term1 :: term2 :: term3 :: Nil) {
    override def toString: String = {
      "IsintNode(subterms:{" + term1.toString + ", " +
      term2.toString + ", " + term3.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      mk_isint(term1.toScala(n2sp),term2.toScala(n2sp),term3.toScala(n2sp))
    }
  }

  // axiom, none, none, none
  case class AxiomNode() extends TreeNode("axiom", "", Nil, Nil) {
    override def toString: String = "AxiomNode()\n"
    override def toScala(n2sp: N2Sparam): String = "Ax"
  }
  // isaxiom, term1, term2, term3
  case class IsaxiomNode(term1: TreeNode, term2: TreeNode, term3: TreeNode) extends TreeNode("isaxiom", "", Nil, term1 :: term2 :: term3 :: Nil) {
    override def toString: String = {
      "IsaxiomNode(subterms:{" + term1.toString + ", " +
      term2.toString + ", " + term3.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      mk_isaxiom(term1.toScala(n2sp),term2.toScala(n2sp),term3.toScala(n2sp))
    }
  }
  // fix, term, none
  case class FixNode(term: TreeNode) extends TreeNode("fix", "", Nil, term :: Nil) {
    override def isFix() : Boolean = true
    override def toString: String = "FixNode(subterms:{" + term.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = {
      "fix(" + term.toScala(n2sp) + ")"
    }
  }
  // bottom, none, none, none
  case class BottomNode() extends TreeNode("bottom", "", Nil, Nil) {
    override def toString: String = "BottomNode()\n"
    override def toScala(n2sp: N2Sparam): String = "Bot"

  }

  // -----------------------------------------------------------------

  // ind, term1, term2, term3, term4
  case class IndNode(term1: TreeNode, param21: NuprlParameter, param22: NuprlParameter, term2: TreeNode, term3: TreeNode, param41: NuprlParameter, param42: NuprlParameter, term4: TreeNode) extends TreeNode("ind", "", Nil, term1 :: (BoundIdNode(param21 :: param22 :: Nil, term2)) :: term3 :: (BoundIdNode(param41 :: param42 :: Nil, term4)) :: Nil) {
    override def toString: String = "IndNode(subterms:{" +
      term1.toString + ", " + term2.toString + ", " + term3.toString + ", " + term4.toString + "})\n"

    override def toScala(n2sp: N2Sparam): String = throw new Exception("IndNode:toScala")
      // {
    //   "{\n" +
    //   "    val x = " + term1.toScala(list) + "\n" +
    //   "    if (x < 0) {\n" +
    //   "    " + term2.toScala(list) + "\n}" +
    //   "    else if (x == 0) {\n "
    //   "    " + term3.toScala(list) + "\n}" +
    //   "    else {\n " +
    //   "    " + term4.toScala(list) + "\n}" +
    //   "\n" +
    //   "}\n"
    // }
  }

  // any, term/none, none, none
  case class AnyNode(term: TreeNode) extends TreeNode("any", "", Nil, term :: Nil) {
    override def toString: String = "AnyNode()\n"
    override def toScala(n2sp: N2Sparam): String = term.toScala(n2sp)
  }

  // token, param, none, none
  case class TokenNode(param: NuprlParameter) extends TreeNode("token", "", param :: Nil, Nil) {
    override def toString: String = "TokenNode(params:{" + param.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = "\"" + param.pvalue + "\""
  }

  // union, term1, term2, none
  case class UnionNode(term1: TreeNode, term2: TreeNode) extends TreeNode("union", "", Nil, term1 :: term2 :: Nil) {
    override def toString: String = "UnionNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = "\"UNION\""
  }

  // rec, term, none
  case class RecNode(term: TreeNode) extends TreeNode("rec", "", Nil, term :: Nil) {
    override def toString: String = "RecNode(subterms:{" + term.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = "\"REC\""
  }

  // callbyvalue, term1, term2, none
  case class CallbyvalueNode(term1: TreeNode, param: NuprlParameter, term2: TreeNode) extends TreeNode("callbyvalue", "", Nil, term1 :: (BoundIdNode(param :: Nil, term2)) :: Nil) {
    override def toString: String = "CallbyvalueNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = {
      "{\n" +
      "val " + param.toScala(n2sp) + " = " + "{\n" + term1.toScala(n2sp) + "}\n" +
      term2.toScala(n2sp) +
      "\n}"
/*      mk_nuprl_apply_cbv(
        mk_fun1_n(param.toScala(n2sp),term2.toScala(n2sp)),
	term1.toScala(n2sp)
      )*/

    }
  }

  // callbyvalueall, term1, term2, none
  case class CallbyvalueallNode(term1: TreeNode, param: NuprlParameter, term2: TreeNode) extends TreeNode("callbyvalueall", "", Nil, term1 :: (BoundIdNode(param :: Nil, term2)) :: Nil) {
    override def toString: String = "CallbyvalueallNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = {
      "{\n" +
      "val " + param.toScala(n2sp) + " = " + "{\n" + term1.toScala(n2sp) + "}\n" +
      term2.toScala(n2sp) +
      "\n}"
    }
  }

  // product, term1, term2, none
  case class ProductNode(term1: TreeNode, term2: TreeNode) extends TreeNode("product", "", Nil, term1 :: term2 :: Nil) {
    override def toString: String = "ProductNode(subterms:{" + term1.toString + ", " + term2.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = "\"PRODUCT\""
  }

  // atom, param, none, none
  case class AtomNode(param: NuprlParameter) extends TreeNode("atom", "", param :: Nil, Nil) {
    override def toString: String = "AtomNode(params:{" + param.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = "\"ATOM\""
  }

  // int, none, none, none
  case class IntNode() extends TreeNode("int", "", Nil, Nil) {
    override def toString: String = "IntNode()\n"
    override def toScala(n2sp: N2Sparam): String = "\"INT\""
  }

  // equal, term1, term2, term3
  case class EqualNode(term1: TreeNode, term2: TreeNode, term3: TreeNode) extends TreeNode("equal", "", Nil, term1 :: term2 :: term3 :: Nil) {
    override def toString: String = "EqualNode(subterms:{" + term1.toString + ", " + term2.toString + ", " + term3.toString + "})\n"
    override def toScala(n2sp: N2Sparam): String = "\"EQUAL\""
  }

  // atom_eq, param, term1, term2, term3, term4
  case class Atom_eqNode(param: NuprlParameter, term1: TreeNode, term2: TreeNode, term3: TreeNode, term4: TreeNode) extends TreeNode("atom_eq", "", param :: Nil, term1 :: term2 :: term3 :: term4 :: Nil) {
    override def toString: String = {
      "Atom_eqNode(params:{" + param.toString + "}, subterms:{" +
      term1.toString + ", " +
      term2.toString + ", " +
      term3.toString + ", " +
      term4.toString + "})\n"
    }
    override def toScala(n2sp: N2Sparam): String = {
      mk_atom_eq(
        term1.toScala(n2sp),
        term2.toScala(n2sp),
        term3.toScala(n2sp),
        term4.toScala(n2sp))
    }
  }

}
