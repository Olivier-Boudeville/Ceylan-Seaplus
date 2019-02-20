% Part of the foobar Erlang-side API that shall be shared between the
% integration module (foobar) and the user code (ex: foobar_test.erl).


% Mirrors foobar's struct foo:
-record( foo, {

	% Some comment about count:
	count :: integer(),

	% Some comment about value:
	% (note that this corresponds to a C double, not the original float)
	%
	value :: float()

} ).


-type foo() :: #foo{}.
