import scala.util.parsing.combinator._
import scala.util.parsing.input.{ StreamReader, Reader }
import scala.util.matching.Regex
import java.io._
import N2Sclass._

/**
 * N2Sparser
 * It contains the information used to build the parser
 */
object N2Sparser extends RegexParsers {

  /**
   * A term is of that form:
   * term ::= {opid:tag[,parameter]*}(subterms)
   * opid ::= seq
   * tag ::= seq
   * parameter ::= value_param:type_param
   * value_param ::= seq
   * type_param ::= seq
   * subterms ::= | term[;term]*
   */
  private def digit = regex(new Regex("[0-9]"))
  private def letter = regex(new Regex("[a-zA-Z]"))
  private def symbol = regex(new Regex("[!_'%-]")) | regex(new Regex("\\\\[^{}()\\s:,;]")) |
    42.toChar | 43.toChar | 47.toChar | 60.toChar | 63.toChar | 64.toChar | 124.toChar
  // *, +, /, ?, @, |, <
  // \{, \}, \(, |), \:, \,, \;
  private def specSymbol = regex(new Regex("\\\\[{}()\\s:,;]"))
  // control character /004   
  private def endEsc = (new Regex(4.toChar + "") | "")
  private def escape = regex(new Regex("[\\s]"))
  private def escapes = (escape*)
  private def seq = (digit | letter | symbol | specSymbol)*

  private def opid = seq ^^ { s => s.mkString("") }
  private def tag = seq ^^ { s => s.mkString("") }
  private def value_param = seq ^^ { s => s.mkString("") }
  private def type_param = seq ^^ { s => s.mkString("") }
  private def parameter = "," ~ value_param ~ ":" ~ type_param ^^ { case comma ~ vp ~ col ~ tp => new NuprlParameter(vp, tp) }
  private def parameters: Parser[List[NuprlParameter]] = (parameter*) ^^ { case p => p.toList }
  private def subterm: Parser[TreeNode] = ";" ~ escapes ~ term ^^ { case semicol ~ esc ~ t => t }
  private def subterm2: Parser[List[TreeNode]] = (subterm*) ^^ { case st => st.toList }
  private def subterms: Parser[List[TreeNode]] =
    (term ~ subterm2 | "") ^^ {
      case (t: TreeNode) ~ (st : List[TreeNode]) => t :: st
      //case (t: TreeNode) => t :: Nil
      case _ => Nil
    }

  private def term: Parser[TreeNode] =
    ((escapes ~ "{") ~> opid ~ (":" ~> tag) ~ (parameters <~ "}") ~ ((escapes ~ "(") ~> subterms) <~ (")" ~ endEsc ~ escapes)
     | (escapes ~ "{}" ~ escapes ~ "(") ~> subterms <~ (")" ~ escapes)) ^^ {
      case (op: String) ~ (tag: String) ~ (p : List[NuprlParameter]) ~ (st: List[TreeNode]) =>
         op match {

           case "bound_id" => // finished
             p match {
               case params =>
                st match {
                  case term :: Nil => new BoundIdNode(params, term)
                  case _ => throw new Exception("bound_id nodes have 1 subterm")
                }
            }

          case "lambda" => // finished
            st match {
              case term :: Nil =>
                term match {
                  case BoundIdNode(param :: Nil, t) => new LambdaNode(param, t)
                  case _ => throw new Exception("lambda terms have 1 bound variable")
                }
              case _ => throw new Exception("lambda nodes have 1 subterm")
            }
          case "apply" =>
            st match {
              case term1 :: term2 :: Nil => new ApplyNode(term1, term2)
              case _ => throw new Exception("apply nodes have 2 subterms")
            }

          case "spread" =>
            st match {
              case term1 :: term2 :: Nil =>
                term2 match {
                  case BoundIdNode(param1 :: param2 :: Nil, t) => new SpreadNode(term1, param1, param2, t)
                  case _ => throw new Exception("spread nodes have 2 params")
                }
              case _ => throw new Exception("spread nodes have 2 subterms")
            }

          case "spreadn" =>
            st match {
              case term1 :: term2 :: Nil =>
                term2 match {
                  case BoundIdNode(param1 :: param2 :: param3 :: Nil, t) =>
		    new SpreadNode(term1,
				   param1,
				   param2,
				   new SpreadNode (new VariableNode(param2),
						   param2,
						   param3,
						   t))
                  case BoundIdNode(param1 :: param2 :: param3 :: param4 :: Nil, t) =>
		    new SpreadNode(term1,
				   param1,
				   param2,
				   new SpreadNode (new VariableNode(param2),
						   param2,
						   param3,
						   new SpreadNode (new VariableNode(param3),
								   param3,
								   param4,
								   t)))
                  case BoundIdNode(param1 :: param2 :: param3 :: param4 :: param5 :: Nil, t) =>
		    new SpreadNode(term1,
				   param1,
				   param2,
				   new SpreadNode (new VariableNode(param2),
						   param2,
						   param3,
						   new SpreadNode (new VariableNode(param3),
								   param3,
								   param4,
								   new SpreadNode (new VariableNode(param4),
										   param4,
										   param5,
										   t))))
                  case BoundIdNode(param1 :: param2 :: param3 :: param4 :: param5 :: param6 :: Nil, t) =>
		    new SpreadNode(term1,
				   param1,
				   param2,
				   new SpreadNode (new VariableNode(param2),
						   param2,
						   param3,
						   new SpreadNode (new VariableNode(param3),
								   param3,
								   param4,
								   new SpreadNode (new VariableNode(param4),
										   param4,
										   param5,
										   new SpreadNode (new VariableNode(param5),
												   param5,
												   param6,
												   t)))))
                  case BoundIdNode(param1 :: param2 :: param3 :: param4 :: param5 :: param6 :: param7 :: Nil, t) =>
		    new SpreadNode(term1,
				   param1,
				   param2,
				   new SpreadNode (new VariableNode(param2),
						   param2,
						   param3,
						   new SpreadNode (new VariableNode(param3),
								   param3,
								   param4,
								   new SpreadNode (new VariableNode(param4),
										   param4,
										   param5,
										   new SpreadNode (new VariableNode(param5),
												   param5,
												   param6,
												   new SpreadNode (new VariableNode(param6),
														   param6,
														   param7,
														   t))))))
                  case _ => throw new Exception("spreadn nodes have 2 params")
                }
              case _ => throw new Exception("spreadn nodes have 2 subterms")
            }

          case "decide" =>
            st match {
              case term1 :: term2 :: term3 :: Nil =>
                term2 match {
                  case BoundIdNode(param1 :: Nil, t1) =>
                    term3 match {
                      case BoundIdNode(param2 :: Nil, t2) => new DecideNode(term1, param1, t1, param2, t2)
                      case _ => throw new Exception("decide term3 has 1 param")
                    }
                  case _ => throw new Exception("decide term2 has 1 param")
                }
              case _ => throw new Exception("decide nodes have 3 subterms")
            }

          case "any" =>
            st match {
              case Nil => new AnyNode(new TermNode("", "", Nil, Nil))
              case term :: Nil => new AnyNode(term)
              // there are other options for any nodes
              case _ => new TermNode(op, tag, p, st)
            }
          case "pair" =>
            st match {
              case term1 :: term2 :: Nil => new PairNode(term1, term2)
              case _ => throw new Exception("pair nodes have 2 subterms")
            }
          case "ispair" =>
            st match {
              case term1 :: term2 :: term3 :: Nil => new IspairNode(term1, term2, term3)
              case _ => throw new Exception("ispair nodes have 3 subterms")
            }
          case "inl" =>
            st match {
              case term :: Nil => new InlNode(term)
              case _ => throw new Exception("inl nodes have 1 subterm")
            }
          case "isinl" =>
            st match {
              case term1 :: term2 :: term3 :: Nil => new IsinlNode(term1, term2, term3)
              case _ => throw new Exception("isinl nodes have 3 subterms")
            }
          case "inr" =>
            st match {
              case term :: Nil => new InrNode(term)
              case _ => throw new Exception("inr nodes have 1 subterm")
            }
          case "isinr" =>
            st match {
              case term1 :: term2 :: term3 :: Nil => new IsinrNode(term1, term2, term3)
              case _ => throw new Exception("isinr nodes have 3 subterms")
            }
          case "natural_number" =>
            p match {
              case param :: Nil => new Natural_numberNode(param)
              case _ => throw new Exception("natural_number nodes have 1 param")
            }
          case "minus" =>
            st match {
              case term :: Nil => new MinusNode(term)
              case _ => throw new Exception("minus nodes have 1 subterm")
            }
          case "add" => // finished
            st match {
              case term1 :: term2 :: Nil => new AddNode(term1, term2)
              case _ => throw new Exception("add nodes have 2 subterms")
            }
          case "subtract" => // finished
            st match {
              case term1 :: term2 :: Nil => new SubtractNode(term1, term2)
              case _ => throw new Exception("subtract nodes have 2 subterms")
            }
          case "multiply" => // finished
            st match {
              case term1 :: term2 :: Nil => new MultiplyNode(term1, term2)
              case _ => throw new Exception("multiply nodes have 2 subterms")
            }
          case "divide" => // finished
            st match {
              case term1 :: term2 :: Nil => new DivideNode(term1, term2)
              case _ => throw new Exception("divide nodes have 2 subterms")
            }
          case "remainder" => // finished
            st match {
              case term1 :: term2 :: Nil => new RemainderNode(term1, term2)
              case _ => throw new Exception("remainder nodes have 2 subterms")
            }
          case "axiom" =>
            st match {
              case Nil => new AxiomNode()
              case _ => throw new Exception("axiom nodes have 0 subterm")
            }
          case "isaxiom" =>
            st match {
              case term1 :: term2 :: term3 :: Nil => new IsaxiomNode(term1, term2, term3)
              case _ => throw new Exception("isaxiom nodes have 3 subterms")
            }
          case "fix" =>
            st match {
              case term :: Nil => new FixNode(term)
              case _ => new TermNode(op, tag, p, st)
            }

          case "ind" =>
            st match {
              case term1 :: term2 :: term3 :: term4 :: Nil =>
                term2 match {
                  case BoundIdNode(param21 :: param22 :: Nil, t2) =>
                    term4 match {
                      case BoundIdNode(param41 :: param42 :: Nil, t4) => new IndNode(term1, param21, param22, t2, term3, param41, param42, t4)
                      case _ => throw new Exception("ind term2 has 2 params")
                    }
                  case _ => throw new Exception("ind term4 has 2 params")
                }
              case _ => throw new Exception("ind nodes have 4 subterms")
            }

          case "int_eq" =>
            st match {
              case term1 :: term2 :: term3 :: term4 :: Nil => new Int_eqNode(term1, term2, term3, term4)
              case _ => throw new Exception("int_eq nodes have 4 subterms")
            }
          case "less" =>
            st match {
              case term1 :: term2 :: term3 :: term4 :: Nil => new LessNode(term1, term2, term3, term4)
              case _ => throw new Exception("less nodes have 4 subterms")
            }
          case "rec" =>
            st match {
              case term :: Nil => new RecNode(term)
              case _ => throw new Exception("rec nodes have 1 subterm")
            }
          case "isint" =>
            st match {
              case term1 :: term2 :: term3 :: Nil => new IsintNode(term1, term2, term3)
              case _ => throw new Exception("isint nodes have 3 subterms")
            }
          case "bottom" =>
            st match {
              case Nil => new BottomNode()
              case _ => new TermNode(op, tag, p, st)
            }
          case "token" =>
            p match {
	      case param :: Nil => new TokenNode(param)
              case _ => throw new Exception("token nodes have 1 param")
	    }

          case "union" =>
            st match { case term1 :: term2 :: Nil => new UnionNode(term1, term2)
                      case term1 :: term2 :: term3 ::term4 :: Nil => new TermNode(op, tag, p, st)
                      case _ => throw new Exception("union nodes have 2 subterms")}

          case "callbyvalue" =>
            st match {
	      case term1 :: term2 :: Nil =>
		term2 match {
                  case BoundIdNode(param :: Nil, t) => new CallbyvalueNode(term1, param, t)
                  case _ => throw new Exception("callbyvalue nodes have 1 param")
                }
              case _ => new TermNode(op, tag, p, st)
	    }

	   // "let" should really be a callbyvalue let
          case "let" =>
            st match {
	      case term1 :: term2 :: Nil =>
		term2 match {
                  case BoundIdNode(param :: Nil, t) => new CallbyvalueNode(term1, param, t)
                  case _ => throw new Exception("let nodes have 1 param")
                }
              case _ => new TermNode(op, tag, p, st)
	    }

          case "callbyvalueall" =>
            st match {
	      case term1 :: term2 :: Nil =>
		term2 match {
                  case BoundIdNode(param :: Nil, t) => new CallbyvalueallNode(term1, param, t)
                  case _ => throw new Exception("callbyvalueall nodes have 1 param")
                }
              case _ => new TermNode(op, tag, p, st)
	    }

          case "atom" =>
            p match {
	      case param :: Nil =>
		st match {
		  case Nil => new AtomNode(param)
                  case _ => throw new Exception("atom nodes have no subterms")
		}
              case Nil => new AtomNode(new NuprlParameter("", ""))
              case _ => throw new Exception("atom nodes have 1 param or no subterms")
	    }

          case "product" =>
            st match {
	      case term1 :: term2 :: Nil => new ProductNode(term1, term2)
              case _ => throw new Exception("product nodes have 2 subterms")
	    }

          case "int" =>
            st match {
	      case Nil => new IntNode()
              case _ => throw new Exception("int nodes have no subterms")
	    }

          case "equal" =>
            st match {
	      case term1 :: term2 :: term3 :: Nil => new EqualNode(term1, term2, term3)
              case _ => throw new Exception("equal nodes have 3 subterms")
	    }

          case "atom_eq" =>
            p match {
	      case param :: Nil =>
		st match {
		  case term1 :: term2 :: term3 :: term4 :: Nil => new Atom_eqNode(param, term1, term2, term3, term4)
                  case _ => throw new Exception("atom_eq nodes have 4 subterms")
		}
              case Nil =>
		st match {
		  case term1 :: term2 :: term3 :: term4 :: Nil => new Atom_eqNode(new NuprlParameter("", ""), term1, term2, term3, term4)
		  case _ => throw new Exception("atom_eq nodes have 4 subterms")
		}
              case _ => throw new Exception("atom_eq nodes have 4 subterms")
	    }

          // other case
          case "variable" => // finished
            p match {
              case param :: Nil =>
                st match {
                  case Nil => new VariableNode(param)
                  case _ => new VarExpNode(param, st)
                }
              case _ => throw new Exception("variable nodes have only 1 param")
            }

          case "!abstraction" =>
            st match {
              case s1 :: s2 :: s3 :: Nil =>
                s1.topid match {
                  case "!condition_cons" => {
                    val termNode = new TermNode(op, tag, p, st)
                    if (s2 != null && s3 != null)
                      lib += s2.topid -> (s2, s3)
                    termNode
                  }
                  case _ => throw new Exception("first name should be !condition_cons")
                }
              case _ => throw new Exception("!abstraction has 3 subterms")
            }

          case _ => {
            val termNode = new TermNode(op, tag, p, st)
            if (st != null && st.length == 3 && st(0).topid == op) {
              /*
               We're probably dealing with an abstraction:
               - 1st subterm is the lhs of the abstraction.
               - 2nd subterm is the rhs of the abstraction.
               - 3rd subterm is a list of well-formedness of the abstraction,
               which we discard.
               */
              lib += op -> (st(0), st(1))
            }
            termNode
          }
        }
      case (st: List[TreeNode]) => new TermNode("", "", Nil, st)

    }
  private def terms: Parser[List[TreeNode]] = (term*) ^^ { case t => t.toList }

  override val skipWhitespace = false // meaningful spaces in our regex

  def getTerms(): Parser[List[TreeNode]] = {
    return terms;
  }


  def main(args: Array[String]) {
    val curTime = System.currentTimeMillis

    println("parsing library file")
    // we parse the alldefs file, it generates the library
    val library = StreamReader(new InputStreamReader(new FileInputStream("alldefs_small")))
    val libout  = parseAll(getTerms(), library)
    //lib = list_class
    println("library file parsed")
    // println("parsing input file " + args(0))
    val endTime = System.currentTimeMillis
    println(endTime - curTime + "ms")

  }


  //val input = StreamReader(new InputStreamReader(new FileInputStream(args(0))))
  // args(0) was "test"
  /**
   * val input = new StringBuilder()
   * for(line <- scala.io.Source.fromFile("alldefs").getLines()) {
   * input.append(line.trim())
   * }
   */
  //println(input)

}
