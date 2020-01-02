% Copyright (C) 2018-2020 Olivier Boudeville
%
% This file is part of the Ceylan-Seaplus tests and examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Erlang-side API (minimal wrapper made available to the user code) in order to
% interact with the foobar service through its driver.
%
% Relies on the service_support common facility that factors out the relevant
% cross-service, generic features.
%
% Refer to inc/foobar.h for the vanilla, C foobar API.
%
-module(foobar).


% Detailed comment for foo_status/0.
-type foo_status() :: 'low_speed' | 'moderate_speed' | 'full_speed'.


% Detailed comment for tur_status/0.
-type tur_status() :: 'tur_value' | 'non_tur_value'.


-export_type([ foo_data/0, foo_status/0, tur_status/0 ]).


% For the foo_data record/type:
-include("foobar.hrl").


% For the Seaplus support:
-include("seaplus.hrl").



% API declaration.
%
% Note that:
%  - the functions below have a spec, yet are not even defined here
%  - no service-specific start/stop functions have been introduced here


% Detailed comment for foo/1.
-spec foo( integer() ) -> integer().


% Detailed comment for bar/2.
-spec bar( float(), foo_status() ) -> foo_data().


% Detailed comment for baz/2.
-spec baz( integer(), text_utils:ustring() ) -> tur_status().


% Detailed comment for tur/0.
-spec tur() -> boolean().


% Detailed comment for frob/1.
-spec frob( tur_status() ) -> text_utils:ustring().


% Nothing more to add!
