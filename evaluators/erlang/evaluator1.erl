-module(evaluator1).
-export([test/0]).


decr_steps(Steps) ->
    if
	Steps == 0 -> erlang:error(decr_steps);
	true -> Steps - 1
    end.

fo_subst(Sub,Term) ->
    B = term:is_variable_term(Term),
    if B    -> Var = term:dest_variable(Term),
	       Res = lists:keyfind(Var,1,Sub),
	       if is_tuple(Res) -> {_,T} = Res, T;
		  true -> Term
	       end;
       true -> {Opr,BTerms} = Term,
	       F = fun ({Vars,Term}) ->
			   G = fun ({V,T}) ->
				       not(lists:member(V,Vars))
					   andalso
					   not(term:is_free_vars(Vars,T))
			       end,
			   Sub2 = lists:filter(G,Sub),
			   {Vars,fo_subst(Sub2,Term)}
		   end,
	       term:mk_term(Opr,lists:map(F,BTerms))
    end.

num_principals(Op) ->
    L1 = ["limited_type_case","minus","isinr","isinl","ispair",
	  "isint","islambda","isatom2","spread","decide","apply",
	  "callbyvalue","callbyvalueall","list_ind","ind"],
    L2 = ["add","subtract","multiply","divide","remainder",
	  "less","int_eq","atom_eq","eq_term"],
    B1 = lists:member(Op,L1),
    B2 = lists:member(Op,L2),
    if
	B1 -> 1;
	B2 -> 2;
	true -> 0
    end.

is_eval_all(Op) -> 
    lists:member(Op,["callbyvalueall","eq_term"]).

next_steps_eval1(Term,Steps0,Principals,Non_principals,Steps) ->
    Op   = term:opid_of_term(Term),
    Lst1 = ["add","subtract","multiply","divide","remainder"],
    B1   = lists:member(Op,Lst1),
    Lst2 = ["less","int_eq"],
    B2   = lists:member(Op,Lst2),
    Lst3 = ["isinr","isinl","ispair","isint","islambda","isatom2"],
    B3   = lists:member(Op,Lst3),
    if
	B1 ->
	    [V1,V2] = Principals,
	    T = primitive:do_primitive_int_op(Op,V1,V2),
	    {T,decr_steps(Steps),false};
	
	Op == "minus" ->
	    [V] = Principals,
	    B = term:is_term("natural_number",V),
	    if
		B -> T = term:mk_simple_term("minus",[V]),
		     {T,decr_steps(Steps),false};
		true -> T = primitive:do_primitive_minus(V),
			{T,decr_steps(Steps),false}
	    end;

	B2 ->
	    [V1,V2] = Principals,
	    [{[],T3},{[],T4}] = Non_principals,
	    B = primitive:do_primitive_cmp(Op,V1,V2),
	    if B    -> {T3,decr_steps(Steps),true};
	       true -> {T4,decr_steps(Steps),true}
	    end;

	Op == "atom_eq" ->
	    N = try term:firstnat(Term) catch error:true -> 0 end,
	    [V1,V2] = Principals,
	    [{[],T3},{[],T4}] = Non_principals,
	    B = primitive:compare_atomn(N,V1,V2),
	    if B    -> {T3,decr_steps(Steps),true};
	       true -> {T4,decr_steps(Steps),true}
	    end;

	Op == "eq_term" ->
	    [V1,V2] = Principals,
	    B1 = primitive:is_complete_primitive_value(V1),
	    B2 = primitive:is_complete_primitive_value(V2),
	    if B1,B2 ->
		    B  = term:alpha_equal_terms(V1,V2),
		    Ax = term:mk_simple_term("axiom",[]),
		    T  = if B    -> term:mk_simple_term("inl",[Ax]);
			    true -> term:mk_simple_term("inr",[Ax])
			 end,
		    {T,decr_steps(Steps),false};
	       true -> erlang:error(eq_term)
	    end;

	B3 ->
	    [V1] = Principals,
	    [{[],T2},{[],T3}] = Non_principals,
	    B = primitive:to_primtive_test(Op,V1),
	    T = if B -> T2; true -> T3 end,
	    {T,decr_steps(Steps),true};

	Op == "spread" ->
	    [Q] = Principals,
	    [{[X,Y],B}] = Non_principals,
	    [U,V] = term:dest_pair(Q),
	    {fo_subst([{X,U},{Y,V}],B),decr_steps(Steps),true};

	Op == "decide" ->
	    [Q] = Principals,
	    [{[X],A},{[Y],B}] = Non_principals,
	    Binl = term:is_term("inl",Q),
	    Binr = term:is_term("inr",Q),
	    T = if Binl -> fo_subst([{X,term:subtermn(1,Q)}],A);
		   Binr -> fo_subst([{Y,term:subtermn(1,Q)}],B);
		   true -> erlang:error(decide)
		end,
	    {T,decr_steps(Steps),true};

	Op == "apply" ->
	    [{[],Arg}] = Non_principals,
	    OpF = term:opid_of_term(term:subtermn(1,Term)),
	    if Steps < 0, OpF == "ycomb" ->
		    {term:mk_apply_term(Arg,Term),decr_steps(Steps),true};
	       true ->
		    [F] = Principals,
		    {X,BF} = term:dest_lambda(F),
		    {fo_subst([{X,Arg}],BF),decr_steps(Steps),true}
	    end;

	Op == "variable" ->
	    {Term,Steps,false};

	true ->
	    {Term,Steps,false}
    end.

eval_list(Steps,Terms,Eval) ->
    F = fun({Vars,T},{Steps,Terms}) ->
		if
		    length(Vars) == 0 ->
			{T1,Steps1} = Eval(T,Steps),
			{Steps1,Terms ++ [T1]};
		    true ->
			erlang:error(not_simple_term)
		end
	end,
    lists:foldl(F, {Steps,[]}, Terms).

eval({Term,Steps},Cbva) ->
    if
	Cbva -> {Term,Steps};
	true ->
	    {Opr,Subterms} = term:dest_term(Term),
	    {Op,_} = Opr,
	    {Principals,Non_principals} = lists:split(num_principals(Op),Subterms),
	    B =  is_eval_all(Op),
	    F = fun(Term,Steps) ->
			if B -> eval(eval({Term,Steps},false),true);
			   true -> eval({Term,Steps},false)
			end
		end,
	    {Steps1,Terms} = eval_list(Steps,Principals,F),
	    Abort = fun() -> {term:mk_term(Opr, lists:map(fun(T) -> {[],T} end,Terms) ++ Non_principals),Steps1}
		    end,
	    try {Term2,Steps2,Ev} = next_steps_eval1(Term,Steps,Terms,Non_principals,Steps1),
		 if Ev -> eval({Term2,Steps2},false);
		    true -> {Term2,Steps2}
		 end
	    catch error:true -> Abort()
	    end
    end.

evaluator1(Term,Steps) ->
    {Answer,Num} = eval({Term,Steps},false),
    {Answer,Steps - Num}.

testcase1() ->
    Lam = term:mk_lambda_term("x",term:mk_variable_term("x")),
    term:mk_apply_term(Lam,term:mk_integer_term(1)).

testcase2() ->
    N = term:mk_integer_term(2),
    M = term:mk_minus_term(N),
    term:mk_minus_term(M).

test() ->
    [evaluator1(testcase1(),-1),
     evaluator1(testcase2(),-1)].
