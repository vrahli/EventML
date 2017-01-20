import java.io.Externalizable
import java.io.ObjectInput
import java.io.ObjectOutput


object N2Sprelude {

  abstract class NVal extends Externalizable

  case class NVPair(var x: NVal, var y: NVal) extends NVal
  {
    def this() = this(null,null)
    override def writeExternal(out: ObjectOutput) = {
      out.writeObject(x)
      out.writeObject(y)
    }
    override def readExternal(in: ObjectInput) = {
      x = in.readObject().asInstanceOf[NVal]
      y = in.readObject().asInstanceOf[NVal]
    }
    override def equals(o: Any) = o match {
      case (that: NVPair) => { that.x.equals(x) && that.y.equals(y) }
      case _ => false
    }
  }

  case class NVInl(var value: NVal) extends NVal
  {
    def this() = this(null)
    override def writeExternal(out: ObjectOutput) = { out.writeObject(value) }
    override def readExternal(in: ObjectInput) = {
      value = in.readObject().asInstanceOf[NVal]
    }
    override def equals(o: Any) = o match {
      case that: NVInl => that.value.equals(value)
      case _ => false
    }
  }

  case class NVInr(var value: NVal) extends NVal
  {
    def this() = this(null)
    override def writeExternal(out: ObjectOutput) = { out.writeObject(value) }
    override def readExternal(in: ObjectInput) = {
      value = in.readObject().asInstanceOf[NVal]
    }
    override def equals(o: Any) = o match {
     case that: NVInr => that.value.equals(value)
      case _ => false
    }
  }

  case class Axiom() extends NVal
  {
    override def writeExternal(out: ObjectOutput) = { }
    override def readExternal(in: ObjectInput) = { }
    override def equals(o: Any) = o match {
      case that: Axiom => true
      case _ => false
    }
  }

  case class NVInt(var i: Int) extends NVal
  {
    def this() = this(0)
    override def writeExternal(out: ObjectOutput) = { out.writeObject(i) }
    override def readExternal(in: ObjectInput) = {
      i = in.readObject().asInstanceOf[Int]
    }
    override def equals(o: Any) = o match {
      case that: NVInt => that.i.equals(i)
      case _ => false
    }
  }

  case class NVStr(var s: String) extends NVal
  {
    def this() = this("")
    override def writeExternal(out: ObjectOutput) = { out.writeObject(s) }
    override def readExternal(in: ObjectInput) = {
      s = in.readObject().asInstanceOf[String]
    }
    override def equals(o: Any) = o match {
      case that: NVStr => that.s.equals(s)
      case _ => false
    }
  }

  //case class Inl(value : Any) extends Serializable
  //case class Inr(value : Any) extends Serializable
  //case class Axiom() extends Serializable

  case class Bottom()
  case class Inl(val value : Any)
  case class Inr(val value : Any)

  def Ax = new Axiom()
  def Bot = new Bottom()
  def nuprlBTrue  = new NVInl(Ax)
  def nuprlBFalse = new NVInr(Ax)

  def nuprl_apply_cbv(f: Any, a: Any) : Any = f.asInstanceOf[Any=>Any](a)

  def nuprl_apply(f: Any, a: => Any) : Any = {
    f.asInstanceOf[(Unit=>Any)=>Any]((_:Unit) => a)
  }

  def nuprl_v(v:Any) : Any = v.asInstanceOf[Unit=>Any](())

  //  def nuprl_v (v: Any) : Any = { nuprl_apply_cbv(v,()) }

  def fix(f:(Unit=>Any)=>Any) : Any = f((_:Unit) => fix(f))

  def sub(x: Any, y: Any) = (x, y) match {
    case (v1:Int,   v2:Int)    => v1 - v2
    case (NVInt(v1),NVInt(v2)) => v1 - v2
    case (v1:Int,   NVInt(v2)) => v1 - v2
    case (NVInt(v1),v2:Int)    => v1 - v2
    case x => { throw new Exception() } }

  def add(x: Any, y: Any) = (x, y) match {
    case (v1:Int,   v2:Int)    => v1 + v2
    case (NVInt(v1),NVInt(v2)) => v1 + v2
    case (v1:Int,   NVInt(v2)) => v1 + v2
    case (NVInt(v1),v2:Int)    => v1 + v2
    case x => { throw new Exception() } }

  def div(x: Any, y: Any) = (x, y) match {
    case (v1:Int,   v2:Int)    => v1 / v2
    case (NVInt(v1),NVInt(v2)) => v1 / v2
    case (v1:Int,   NVInt(v2)) => v1 / v2
    case (NVInt(v1),v2:Int)    => v1 / v2
    case x => { throw new Exception() } }

  def mul(x: Any, y: Any) = (x, y) match {
    case (v1:Int,   v2:Int)    => v1 * v2
    case (NVInt(v1),NVInt(v2)) => v1 * v2
    case (v1:Int,   NVInt(v2)) => v1 * v2
    case (NVInt(v1),v2:Int)    => v1 * v2
    case x => { throw new Exception() } }

  def rem(x: Any, y: Any) = (x, y) match {
    case (v1:Int,   v2:Int)    => v1 % v2
    case (NVInt(v1),NVInt(v2)) => v1 % v2
    case (v1:Int,   NVInt(v2)) => v1 % v2
    case (NVInt(v1),v2:Int)    => v1 % v2
    case x => { throw new Exception() } }

  def spread(p: Any, f: (Any,Any) => Any) = {
    p match {
      case (a:Any,b:Any) => f(a,b)
      case NVPair(a,b) => f(a,b)
      case _ => throw new Exception("spread is expecting a pair")
    }
  }

  def decide(d: Any, f1: Any => Any, f2: Any => Any) = {
    d match {
      case Inl(a) => f1(a)
      case Inr(b) => f2(b)
      case NVInl(a) => f1(a)
      case NVInr(b) => f2(b)
      case _ => throw new Exception("decide is expecting either an injection")
    }
  }

  def ispair(a: Any, b: => Any, c: => Any) = {
    a match {
      case (_:Any,_:Any) => b
      case NVPair(_,_) => b
      case _ => c
    }
  }

  def isinl(a: Any, b: => Any, c: => Any) = {
    a match {
      case Inl(_) => b
      case NVInl(_) => b
      case _ => c
    }
  }

  def isinr(a: Any, b: => Any, c: => Any) = {
    a match {
      case Inr(_) => b
      case NVInr(_) => b
      case _ => c
    }
  }

  def isint(a: Any, b: => Any, c: => Any) = {
    a match {
      case (_: NVInt) => b
      case (_: Int) => b
      case _ => c
    }
  }

  def isaxiom(a: Any, b: => Any, c: => Any) = {
    a match {
      case Axiom() => b
      case _ => c
    }
  }

  def to_string(a: Any) : String= {
    a match {
      case (x: String) => x
      case NVStr(x) => x
    }
  }

  def atom_eq(a: Any, b: Any, c: => Any, d: => Any) = {
    (a,b) match {
      case (s1: String, s2: String) => if (s1 == s2) { c } else { d }
      case (NVStr(s1),  NVStr(s2))  => if (s1 == s2) { c } else { d }
      case (s1: String, NVStr(s2))  => if (s1 == s2) { c } else { d }
      case (NVStr(s1),  s2: String) => if (s1 == s2) { c } else { d }
    }
  }

  def int_eq(a: Any, b: Any, c: => Any, d: => Any) = {
    (a,b) match {
      case (i1: Int,   i2: Int)   => if (i1 == i2) { c } else { d }
      case (NVInt(i1), NVInt(i2)) => if (i1 == i2) { c } else { d }
      case (i1: Int,   NVInt(i2)) => if (i1 == i2) { c } else { d }
      case (NVInt(i1), i2: Int)   => if (i1 == i2) { c } else { d }
    }
  }

  def less(a: Any, b: Any, c: => Any, d: => Any) = {
    (a,b) match {
      case (i1: Int,   i2: Int)   => if (i1 < i2) { c } else { d }
      case (NVInt(i1), NVInt(i2)) => if (i1 < i2) { c } else { d }
      case (i1: Int,   NVInt(i2)) => if (i1 < i2) { c } else { d }
      case (NVInt(i1), i2: Int)   => if (i1 < i2) { c } else { d }
    }
  }

/*
  def nuprl_tok_deq : Any =
    (x : String) => (y : String) =>
      if (x == y) { new Inl(new Axiom()) }
      else { new Inr(new Axiom()) }

  def nuprl_list_deq(deq : Any) : Any =
    (x : Any) => (y : Any) =>
      (x, y) match {
	case (Axiom(), Axiom()) => Inl(new Axiom())
	case ((a,b), (c,d)) =>
	  (deq.asInstanceOf[Any => Any](a)).asInstanceOf[Any => Any](c) match {
	    case Inl(x) => ((nuprl_list_deq(deq)).asInstanceOf[Any => Any](b)).asInstanceOf[Any => Any](d)
	    case Inr(x) => new Inr(x)
	    case _ => { throw new Exception("nuprl_list_deq(1)") }
	  }
	case (Axiom(), (a,b)) => new Inr(new Axiom())
	case ((a,b), Axiom()) => new Inr(new Axiom())
	case _ => { throw new Exception("nuprl_list_deq(2)") }
      }
*/

  def toNuprlBool(b : Boolean) : Any = {
    if (b) {nuprlBTrue} else {nuprlBFalse}
  }

  def band(b1:Any,b2:Any) : Any = {
    (b1,b2) match {
      case (Inl(_),   Inl(_))   => nuprlBTrue
      case (NVInl(_), NVInl(_)) => nuprlBTrue
      case (NVInl(_), Inl(_))   => nuprlBTrue
      case (Inl(_),   NVInl(_)) => nuprlBTrue
      case _ => nuprlBFalse
    }
  }

  def nuprlTokDeq : (Any => Any => Any) = {
    (x : Any) =>
    (y : Any) =>
    (x,y) match {
      case (s1: String, s2: String) => toNuprlBool(s1 == s2)
      case (NVStr(s1),  NVStr(s2))  => toNuprlBool(s1 == s2)
      case (s1: String, NVStr(s2))  => toNuprlBool(s1 == s2)
      case (NVStr(s1),  s2: String) => toNuprlBool(s1 == s2)
    }
  }

  def nuprlListDeqBody(deq:Any,h1:Any,h2:Any,t1:Any,t2:Any,f:Any => Any => Any) = {
    (nuprl_apply_cbv(nuprl_apply_cbv(deq,h1),h2)) match {
      case Inl(_) => nuprlBTrue
      case NVInl(_) => nuprlBTrue
      case _ => nuprl_apply(nuprl_apply(f,t1),t2)
    }
  }

  def nuprlListDeq(deq: Any) : (Any => Any => Any) = {
    (lst1 : Any) =>
    (lst2 : Any) =>
    (nuprl_v(lst1), nuprl_v(lst2)) match {
      case ((h1,t1),       (h2,t2))       => nuprlListDeqBody(deq,h1,h2,t1,t2,nuprlListDeq(deq))
      case (NVPair(h1,t1), NVPair(h2,t2)) => nuprlListDeqBody(deq,h1,h2,t1,t2,nuprlListDeq(deq))
      case ((h1,t1),       NVPair(h2,t2)) => nuprlListDeqBody(deq,h1,h2,t1,t2,nuprlListDeq(deq))
      case (NVPair(h1,t1), (h2,t2))       => nuprlListDeqBody(deq,h1,h2,t1,t2,nuprlListDeq(deq))
      case (Axiom(),Axiom()) => nuprlBTrue
      case _ => nuprlBFalse
    }
  }

  def nuprlList2scalaList(lst: Any) : List[Any] = {
    lst match {
      case (h,t) => h :: nuprlList2scalaList(t)
      case NVPair(h,t) => h :: nuprlList2scalaList(t)
      case Axiom() => Nil
      case _ => throw new Exception("not a list: " + lst)
    }
  }

    def cast2NVal(m:Any) : NVal = {
      m match {
        case (a,b) => NVPair(cast2NVal(a),cast2NVal(b))
        case Inl(a) => NVInl(cast2NVal(a))
        case Inr(a) => NVInr(cast2NVal(a))
        case (x : Int) => NVInt(x.asInstanceOf[Int])
        case (x : String) => NVStr(x.asInstanceOf[String])
        case (x : NVal) => x
        case x => throw new Exception("castMsg:wrong_format:" + x)
      }
    }

  def main(args: Array[String]) { }

}

