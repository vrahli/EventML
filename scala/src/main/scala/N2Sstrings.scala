/**

  This provides string versions of some functions declared in
  N2Sprelude

  */
object N2Sstrings {

  def mk_one_lazy_param(a: String) = a + ": => Any"
  def mk_param_any(a: String) = "(" + a + " : Any)"
  def mk_param_lazy(a: String) = "(" + a + " : Unit => Any)"
  def mk_pair_any(a: String, b: String) = "(" + a + " : Any ," + b + " : Any)"
  def mk_fun1(v: String, b: String) = mk_param_any(v) + " => " + b
  def mk_fun2(v1: String, v2: String, b: String) = mk_pair_any(v1,v2) + " => " + b
  def mk_fun1_n(v: String, b: String) = mk_param_any(v) + " =>\n " + b
  def mk_fun1_lazy_n(v: String, b: String) = mk_param_lazy(v) + " =>\n " + b
  def mk_fun2_n(v1: String, v2: String, b: String) = mk_pair_any(v1,v2) + " =>\n " + b

  def mk_pair(a: String, b: String) = "(" + a + "," + b + ")"

  def mk_spread1(p: String, p1: String, p2: String, b: String) =
    "spread(" + p + "," + mk_fun2(p1,p2,b) + ")"

  def mk_spread(p: String, p1: String, p2: String, b: String) = {
    "((" + p + ")" +
    " match { case NVPair(x,y) => (x,y) case x => x })" + "\n" +
    " match {" + "\n" +
    "  case (" + p1 + "," + p2 + ") => " + b + "\n" +
    "  case x => throw new Exception(\"spread expects a pair: \" + x)" + "\n" +
    "}"
  }

  def mk_decide1(d: String, p1: String, b1: String, p2: String, b2: String) =
    "decide(" + d + "," + mk_fun1(p1,b1) + "," + mk_fun1(p2,b2) + ")"

  def mk_decide(d: String, p1: String, b1: String, p2: String, b2: String) = {
    "((" + d + ")" + "\n" +
    " match { case NVInl(x) => Inl(x) case NVInr(x) => Inr(x) case x => x })" + "\n" +
    " match {" + "\n" +
    "  case Inl(" + p1 + ") => " + b1 + "\n" +
    "  case Inr(" + p2 + ") => " + b2 + "\n" +
    "  case (x:Any) => throw new Exception(\"decide expects an injection: \" + x)" + "\n" +
    "}"
  }

  def mk_ispair (a: String, b: String, c: String) = "ispair("  + a + "," + b + "," + c + ")"
  def mk_isinl  (a: String, b: String, c: String) = "isinl("   + a + "," + b + "," + c + ")"
  def mk_isinr  (a: String, b: String, c: String) = "isinr("   + a + "," + b + "," + c + ")"
  def mk_isint  (a: String, b: String, c: String) = "isint("   + a + "," + b + "," + c + ")"
  def mk_isaxiom(a: String, b: String, c: String) = "isaxiom(" + a + "," + b + "," + c + ")"

  def mk_int_eq (a: String, b: String, c: String, d: String) = "int_eq("  + a + "," + b + "," + c + "," + d + ")"
  def mk_atom_eq(a: String, b: String, c: String, d: String) = "atom_eq(" + a + "," + b + "," + c + "," + d + ")"
  def mk_less   (a: String, b: String, c: String, d: String) = "less("    + a + "," + b + "," + c + "," + d + ")"

  def mk_nuprl_apply_cbv1(f: String, a: String) = "nuprl_apply_cbv(" + f + "," + a + ")"

  def mk_nuprl_apply_cbv(f: String, a: String) = {
    "(" + f + ").asInstanceOf[Any=>Any](" + a + ")"
  }

  def mk_nuprl_apply1(f: String, a: String) = "nuprl_apply(" + f + "," + a + ")"

  def mk_nuprl_apply(f: String, a: String) = {
    "(" + f + ").asInstanceOf[(Unit=>Any)=>Any]((_:Unit) =>" + a + ")"
  }

  def mk_nuprl_v1(v: String) = "nuprl_v(" + v + ")"

  def mk_nuprl_v(v: String) = "(" + v + ").asInstanceOf[Unit=>Any](())"

  def mk_nuprl_lazy_v(v: String) = v + "(())"

  def replace_str(s : String) = {
    "n2s_" + s.replace("'","pp").replace("-","__").replace("@","AT")
  }

  def main(args: Array[String]) = { }

}
