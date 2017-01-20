-module(term).

-export([test/0]).

-export([opid_of_term/1]).
-export([subtermn/2]).
-export([firstnat/1]).
-export([parameters_of_term/1]).
-export([type_of_parameter/1]).
-export([equal_parameters/2]).

-export([mk_apply_term/2]).
-export([mk_minus_term/1]).
-export([mk_lambda_term/2]).
-export([mk_integer_term/1]).
-export([mk_variable_term/1]).
-export([mk_simple_term/2]).

-export([dest_term/1]).
-export([dest_integer/1]).
-export([dest_lambda/1]).
-export([dest_variable/1]).

-export([is_variable_term/1]).
-export([is_pair_term/1]).
-export([is_lambda_term/1]).
-export([is_free_vars/2]).
-export([alpha_equal_terms/2]).
-export([is_term/2]).

-export([to_string/1]).
-export([print/1]).


%%%% TYPES

-opaque variable() :: string().

-opaque opid() :: string().

-opaque parameter_value() :: string().
-opaque parameter_kind()  :: string().

-opaque parameter() :: {parameter_value(), parameter_kind()}.

-opaque operator() :: {opid(), [parameter()]}.

-opaque nuprl_term() :: {operator(), [{[variable()],nuprl_term()}]}.


%%%% ACCESSORS

-spec opid_of_operator(operator()) -> opid().
opid_of_operator ({Opid, _}) ->
    Opid.

-spec opid_of_term(term()) -> opid().
opid_of_term ({Operator,_}) ->
    opid_of_operator(Operator).

-spec subtermn(integer(),nuprl_term()) -> nuprl_term().
subtermn(N,{_,BTerms}) ->
    {_,Term} = lists:nth(N,BTerms),
    Term.

type_of_parameter({_,K}) -> K.

equal_parameters({V1,K1},{V2,K2}) -> (V1 == V2) and (K1 == K2).

parameters_of_term({{_,Params},_}) -> Params.

destruct_natural_parameter({I,"n"}) -> I.

firstnat(Term) ->
    destruct_natural_parameter(hd(parameters_of_term(Term))).


%%%% SIMPLE CHECKING FUNCTIONS

-spec is_term(opid(),nuprl_term()) -> boolean().
is_term(Token,Term) ->
    string:equal(opid_of_term(Term), Token).

-spec mk_primitive_op_instance_recognizer(opid(),nuprl_term()) -> boolean().
mk_primitive_op_instance_recognizer(Token,Term) ->
    is_term(Token,Term).

-spec is_natural_number_term(nuprl_term()) -> boolean().
is_natural_number_term(Term) ->
    mk_primitive_op_instance_recognizer("natural_number",Term).

-spec is_minus_term(nuprl_term()) -> boolean().
is_minus_term(Term) ->
    mk_primitive_op_instance_recognizer("minus",Term).

-spec is_pair_term(nuprl_term()) -> boolean().
is_pair_term(Term) ->
    mk_primitive_op_instance_recognizer("pair",Term).

-spec is_lambda_term(nuprl_term()) -> boolean().
is_lambda_term(Term) ->
    mk_primitive_op_instance_recognizer("lambda",Term).

-spec is_variable_term(nuprl_term()) -> boolean().
is_variable_term(Term) ->
    mk_primitive_op_instance_recognizer("variable",Term).


%%%% CONSTRUCTORS

-spec mk_parameter(parameter_value(),parameter_kind()) -> parameter().
mk_parameter (Value,Kind) ->
    {Value,Kind}.

-spec mk_variable_parameter(string()) -> parameter().
mk_variable_parameter(Var) ->
    mk_parameter(Var, "v").

-spec mk_natural_number_parameter(integer()) -> parameter().
mk_natural_number_parameter(Tag) ->
    mk_parameter(integer_to_list(Tag), "n").

%% -spec mk_token_parameter(string()) -> parameter().
%% mk_token_parameter(Token) ->
%%     mk_parameter(Token, "t").

%% -spec mk_string_parameter(string()) -> parameter().
%% mk_string_parameter(String) ->
%%     mk_parameter(String, "s").

-spec mk_operator(opid(),[parameter()]) -> operator().
mk_operator (Opid, Params) ->
    {Opid, Params}.

-spec mk_simple_operator(opid()) -> operator().
mk_simple_operator (Opid) ->
    mk_operator(Opid,[]).

-spec mk_term(operator(),[{[variable()],nuprl_term()}]) -> nuprl_term().
mk_term(Opr,BTerms) ->
    {Opr,BTerms}.

-spec mk_simple_term(opid(),[nuprl_term()]) -> nuprl_term().
mk_simple_term (Opid, Terms) ->
    Operator = mk_simple_operator(Opid),
    Bterms   = lists:map(fun(Term) -> {[],Term} end, Terms),
    {Operator,Bterms}.

-spec mk_apply_term(nuprl_term(),nuprl_term()) -> nuprl_term().
mk_apply_term(Function,Argument) ->
    mk_simple_term("apply",[Function,Argument]).

-spec mk_lambda_term(variable(),nuprl_term()) -> nuprl_term().
mk_lambda_term(Var,Body) ->
    mk_term({"lambda",[]},[{[Var],Body}]).

-spec mk_natural_number_term(integer()) -> nuprl_term().
mk_natural_number_term(Int) ->
    mk_term({"natural_number",[mk_natural_number_parameter(Int)]},[]).

-spec mk_minus_term(nuprl_term()) -> nuprl_term().
mk_minus_term(Int) ->
    mk_simple_term("minus",[Int]).

-spec mk_integer_term(integer()) -> nuprl_term().
mk_integer_term(Int) ->
    if
	Int < 0 -> mk_minus_term(mk_natural_number_term(-Int));
	true    -> mk_natural_number_term(Int)
    end.

-spec mk_variable_term(variable()) -> nuprl_term().
mk_variable_term(Var) ->
    mk_term({"variable",[mk_variable_parameter(Var)]},[]).


%%%% DESTRUCTORS

-spec dest_term(nuprl_term()) -> {operator(),[{[variable()],nuprl_term()}]}.
dest_term({Opr,BTerms}) -> {Opr,BTerms}.

-spec dest_minus(nuprl_term()) -> nuprl_term().
dest_minus({{"minus",[]},[{[],Term}]}) -> Term.

-spec dest_lambda(nuprl_term()) -> {variable(),nuprl_term()}.
dest_lambda({{"lambda",[]},[{[Var],Body}]}) -> {Var,Body}.

-spec dest_variable(nuprl_term()) -> parameter_value().
dest_variable({{"variable",[{V,_}]},[]}) -> V.

-spec dest_natural_number(nuprl_term()) -> integer().
dest_natural_number({{"natural_number",[{V,_}]},[]}) ->
    element(1,string:to_integer(V)).

-spec dest_integer(nuprl_term()) -> integer().
dest_integer(Term) ->
    B1 = is_natural_number_term(Term),
    B2 = is_minus_term(Term),
    if B1   -> dest_natural_number(Term);
       B2   -> -dest_natural_number(dest_minus(Term));
       true -> erlang:error(do_primitive_int_op)
    end.


%%%% TO STRING

seq(X,Y,Z) -> string:concat(X,string:concat(Y,Z)).

paren(X)  -> seq("(",X,")").
curly(X)  -> seq("{",X,"}").
square(X) -> seq("[",X,"]").

sep([],_,_) -> "";
sep([X|[]],_,F) -> F(X);
sep([Head|Tail],Sep,F) -> seq(F(Head),Sep,sep(Tail,Sep,F)).

sep_id(Lst,Sep) -> sep(Lst,Sep,fun(X) -> X end).

to_string_param({Value,Kind}) -> sep_id([Value,Kind],":").

to_string_params(Params) ->
    curly(sep(Params,";",fun(X) -> to_string_param(X) end)).

to_string_operator({Opid,Params}) ->
    string:concat(Opid,to_string_params(Params)).

to_string_vars(Vars) -> square(sep_id(Vars,";")).

to_string(Term) ->
    {Opr,BTerms} = Term,
    F = fun({Vs,T}) -> sep_id([to_string_vars(Vs),to_string(T)],",") end,
    S = paren(sep(BTerms,";",F)),
    string:concat(to_string_operator(Opr),S).

print(Term) -> io:fwrite(string:concat(to_string(Term),"\n")).


%%%% OTHER

%% is_free_var(Var,Term) ->
%%     B = is_variable_term(Term),
%%     if B    -> Var == dest_variable(Term);
%%        true -> {Opr,BTerms} = Term,
%% 	       F = fun (Vars,T) -> not(list:member(Var,Vars)) and is_free_var(Var,T) end,
%% 	       lists:any(F,BTerms)
%%     end.

is_free_vars(Vs,Term) ->
    B = is_variable_term(Term),
    if B    -> lists:member(dest_variable(Term),Vs);
       true -> {_,BTerms} = Term,
	       F = fun (Vars,T) ->
			   G = fun(X) -> not(lists:member(X,Vars)) end,
			   Vs2 = lists:filter(G,Vs),
			   if length(Vs2) == 0 -> false;
			      true -> is_free_vars(Vs2,T)
			   end
		   end,
	       lists:any(F,BTerms)
    end.

alpha_equal_terms(Term1,Term2,Sub) ->
    {{Op1,Params1},BTerms1} = Term1,
    {{Op2,Params2},BTerms2} = Term2,
    P = fun ({Vs1,T1},{Vs2,T2}) ->
		(length(Vs1) == length(Vs2)
		 andalso
		 alpha_equal_terms(T1,T2,lists:zip(Vs2,Vs1)++Sub))
	end,
    ApplySub =
	fun(S,V) ->
		T = lists:keyfind(V,1,S),
		if is_tuple(T) -> element(2,T); true -> V end
	end,
    ((is_variable_term(Term1)
      andalso is_variable_term(Term2)
      andalso dest_variable(Term1) == ApplySub(Sub,dest_variable(Term2)))
     orelse
       (Op1 == Op2
	andalso Params1 == Params2
	andalso length(BTerms1) == length(BTerms2)
	andalso lists:all(P,lists:zip(BTerms1,BTerms2)))).

alpha_equal_terms(Term1,Term2) -> alpha_equal_terms(Term1,Term2,[]).

%%%% TESTING

test() ->
    mk_simple_term("foobar",[]).
