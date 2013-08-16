%% Feel free to use, reuse and abuse the code in this file.

-module(re_utils).

-export([
		 compile/1,
		 captures/2, captures/3,
		 groups/1
		]).

-opaque capture_re() :: {re:mp(), [atom()]}.
-type re_opts() :: [any()]. % re opts

-export_type([capture_re/0]).

-spec compile(Re :: iodata()) -> {ok, capture_re()} | {error, ErrSpec} when
	  ErrSpec :: {ErrString, Position},
	  ErrString :: string(),
	  Position :: non_neg_integer().
compile(Re) ->
	case re:compile(Re) of
		{ok, CompiledRe} ->
			Groups = groups(Re),
			{ok, {CompiledRe, Groups}};
		{error, _} = Err ->
			Err
	end.

-spec captures(Subject, Re) -> {match, [{atom(), binary()}]} | nomatch when
	  Subject :: iodata(),
	  Re :: iodata() | capture_re().
captures(Subject, Re) ->
	captures(Subject, Re, []).

-spec captures(Subject, Re, Options) -> {match, [{atom(), binary()}]} | nomatch when
	  Options :: re_opts(),
	  Subject :: iodata(),
	  Re :: iodata() | capture_re().
captures(Subject, {Re, Groups}, Opts) ->
	Opts2 = [{capture, Groups, binary} | Opts],
	case re:run(Subject, Re, Opts2) of
		match ->
			{match, []};
		{match, Result} ->
			{match, lists:zip(Groups, Result)};
		nomatch ->
			nomatch
	end;
captures(Subject, Re, Opts) ->
	captures(Subject, {Re, groups(Re)}, Opts).

-spec groups(Re :: iodata()) -> [BindingName :: atom()].
groups(Re) when is_list(Re) ->
	groups(iolist_to_binary(Re));
groups(Bin) when is_binary(Bin) ->
	groups(Bin, []).

groups(<<>>, Acc) ->
	lists:reverse(Acc);
groups(<<"(?<", Rest/binary>>, Acc) ->
	word(Rest, fun(Bin, <<">", R/binary>>) when Bin =/= <<>>  ->
					   Binding = list_to_atom(binary_to_list(Bin)),
					   groups(R, [Binding|Acc]);
				  (_, R) ->
					   groups(R, Acc)
			   end);
groups(<<"\\\\", Rest/binary>>, Acc) ->
	groups(Rest, Acc);
groups(<<"\\(", Rest/binary>>, Acc) ->
	groups(Rest, Acc);
groups(<<_C, Rest/binary>>, Acc) ->
	groups(Rest, Acc).

-spec word(Bin, fun((Word, RestBin) -> Result)) -> Result when
	  Bin :: binary(),
	  RestBin :: binary(),
	  Word :: binary().
word(Bin, Fun) ->
	word(Bin, Fun, <<>>).

word(<<C, Rest/binary>>, Fun, Acc) when C >= $a, C =< $z;
										C >= $A, C =< $Z;
										C >= $0, C =< $9;
										C =:= $_ ->
	word(Rest, Fun, <<Acc/binary, C>>);
word(Rest, Fun, Acc) ->
	Fun(Acc, Rest).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

group_names_test_() ->
	Tests = [
		{"(?<test>.*)", [test]},
		{"pre(?<test>.*)", [test]},
		{"(?<test>.*)post", [test]},
		{"(?<test1>.*)multipost(?<test2>.*)", [test1, test2]},
		{"(((?<test>asdf)))", [test]},
		{"(((?<outer>a(?<inner>sd)f)))", [outer, inner]},
		{"\\(?<test>.*)", []},
		{"\\\\(?<test>.*)", [test]},
		{"\\\\\\(?<test>.*)", []},
		{"\\\\\\\\(?<test>.*)", [test]},
		{"\\\\\\\\\\(?<test>.*)", []},
		{"(\\?<test>.*)", []}
	],
	[fun() -> R = groups(D) end || {D, R} <- Tests].

captures_test_() ->
	Tests = [
		{"(?<a>a)", "a", [{a, <<"a">>}]},
		{"(?<a>a)(?<b>b)", "ab", [{a, <<"a">>}, {b, <<"b">>}]},
		{"(?<a>a) middle (?<b>b)", "a middle b", [{a, <<"a">>}, {b, <<"b">>}]},
		{"(?<outer>a(?<inner>b))", "ab", [{outer, <<"ab">>}, {inner, <<"b">>}]},
		{"(?<outer>a(?<inner>b))(?<external>c)", "abc",
		[{outer, <<"ab">>}, {inner, <<"b">>}, {external, <<"c">>}]}
	],
	CompiledTests = [{compile(Re), S, R} || {Re, S, R} <- Tests],
	[fun() -> {match, R} = captures(S, Re) end || {Re, S, R} <- Tests ++ CompiledTests].

-endif.

