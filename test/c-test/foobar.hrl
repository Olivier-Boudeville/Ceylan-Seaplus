% Copyright (C) 2018-2023 Olivier Boudeville
%
% This file is part of the Ceylan-Seaplus tests and examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Part of the foobar Erlang-side API that shall be shared between the
% integration module (foobar.erl) and the user code (ex: foobar_test.erl).


% Mirrors foobar's struct foo_data:
-record( foo_data, {

	% Some comment about count:
	count :: integer(),

	% Some comment about value:
	% (note that this corresponds to a C double - not the original float)
	%
	value :: float()

} ).

-type foo_data() :: #foo_data{}.
