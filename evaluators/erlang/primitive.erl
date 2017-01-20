-module(primitive).
-export([do_primitive_int_op/3]).
-export([do_primitive_cmp/3]).
-export([do_primitive_minus/1]).
-export([compare_atomn/3]).


do_primitive_int_op(Op,Term1,Term2) ->
    N1 = term:dest_integer(Term1),
    N2 = term:dest_integer(Term2),
    Z  = if
	     Op == "add"       -> N1 + N2;
	     Op == "subtract"  -> N1 - N2;
	     Op == "multiply"  -> N1 * N2;
	     Op == "divide"    -> N1 div N2;
	     Op == "remainder" -> N1 rem N2;
	     true -> erlang:error(do_primitive_int_op)
	 end,
    term:mk_integer_term(Z).

do_primitive_cmp(Cmp,Term1,Term2) ->
    N1 = term:dest_integer(Term1),
    N2 = term:dest_integer(Term2),
    if
	Cmp == "int_eq" -> N1 == N2;
	Cmp == "less"   -> N1 < N2;
	true -> erlang:error(do_primitive_cmp)
    end.

do_primitive_minus(I) ->
    N = term:dest_integer(I),
    term:mk_integer_term(-N).

compare_atomn(N,V1,V2) ->
    Opid1  = term:opid_of_term(V1),
    Opid2  = term:opid_of_term(V1),
    [P1]   = term:parameters_of_term(V1),
    [P2]   = term:parameters_of_term(V2),
    PType1 = term:type_of_parameter(P1),
    PType2 = term:type_of_parameter(P2),
    B = if N == 0 -> PType1 == "token";
	   N == 1 -> PType1 == "ut1";
	   N == 2 -> PType2 == "ut2";
	   true -> false
	end,
    if Opid1 == "token",
       Opid2 == "token",
       PType1 == PType2,
       B -> term:equal_parameters(P1,P2);
       true -> erlang:error(compare_atomn)
    end.
       
