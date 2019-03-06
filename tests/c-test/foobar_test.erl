% Copyright (C) 2018-2019 Olivier Boudeville
%
% This file is part of the Ceylan-Seaplus library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: December 16, 2018



% Allows to test the full chain, from the initial service call to the obtaining
% of its result.
%
% Erlang counterpart translation of foobar_test.c, with additions at the end.
%
-module(foobar_test).


-export([ run/0 ]).


% For the foo_data record:
-include("foobar.hrl").


run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the Erlang-integrated foobar service." ),

	% Not foobar:start_link(), as here we want to survive a crash of the foobar
	% service (i.e. to be able to handle failures explicitly, as messages
	% received by this test process):
	%
	foobar:start(),

	MyFooData = foobar:bar( 3.14, full_speed ),

	NewCount = foobar:foo( MyFooData#foo_data.count ),

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

	catch throw:{ driver_crashed, ErrorReason } ->

		test_facilities:display( "Exception thrown as expected: ~p",
								 [ ErrorReason ] ),

		% Check (best option: read the seaplus log to investigate real crashes):
		unknown_reason = ErrorReason,

		true

	end,

	case FooCrashed of

		true ->
			ok;

		false ->
			throw( foo_exception_not_raised )

	end,

	foobar:restart(),


	#foo_data{ count=4, value = -20.0 } = foobar:bar( 2.0, moderate_speed ),

	tur_value = foobar:baz( 10, "cat" ),
	non_tur_value = foobar:baz( 7, "dog" ),

	true = foobar:tur(),

	"this is tur" = foobar:frob( tur_value ),
	"this is non-tur" = foobar:frob( non_tur_value ),

	% Add a test with a returned binary string:
	%<<"My beautiful binary">> = foobar:frob( "beautiful" ),


	foobar:stop(),

	test_facilities:stop().
