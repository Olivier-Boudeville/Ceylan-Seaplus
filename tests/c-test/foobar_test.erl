% Allows to test the full chain, from the initial service call to the obtaining
% of its result.
%
% Erlang counterpart of foobar_test.c, with additions.
%
-module(foobar_test).


-export([ run/0 ]).




run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the Erlang-integrated foobar service." ),

	% Not foobar:start_link(), as here we want to survive a crash of the foobar
	% service (i.e. to be able to handle failures explicitly from the test
	% process):
	%
	foobar:start(),

	MyFooRecord = foobar:bar( 3.14, full_speed ),

	NewCount = foobar:foo( MyFooRecord#foo.count ),

	Res = case foobar:tur() of

		true ->
			foobar:baz( NewCount, "Hello" ) ;

		false ->
			non_tur_value

	end,

	io:format( "Having: ~s~n", [ foobar:frob( Res ) ] ),


	test_facilities:display( "Now, some more extensive, extra testing." ),

	% Better than { ok, 4 }, as we rely on exception support:
	4 = foobar:foo( 3 ),

	% Throwing an exception is better than returning { error, FailReason }:
	FooCrashed = try

		% Expected to crash:
		foobar:foo( 0 ),
		false

	catch _:{ call_failed, ErrorReason } ->

		test_facilities:display( "Exception thrown as expected:~n~p",
								 [ ErrorReason ] ),
		true

	end

	case FooCrashed of

		true ->
			ok;

		false ->
			throw( foo_exception_not_raised )

	end,

	foobar:restart(),


	#foo{ count=4, value = -20.0 } = foobar:bar( 2.0, moderate_speed ),

	tur_value = foobar:baz( 10, "cat" ),
	non_tur_value = foobar:baz( 7, "dog" ),

	true = foobar:tur(),

	"this is tur" = foobar:frob( tur_value ),
	"this is non-tur" = foobar:frob( non_tur_value ),

	% Add a test with a returned binary string:
	%<<"My beautiful binary">> = foobar:frob( "beautiful" ),


	foobar:stop(),

	test_facilities:stop().
