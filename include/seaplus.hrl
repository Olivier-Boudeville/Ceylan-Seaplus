% Copyright (C) 2018-2020 Olivier Boudeville
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



% This header file auto-defines the Seaplus base, generic service functions.
%
% It is meant to be included by the bridging modules (ex: for a Foobar service,
% by foobar.erl).


-define( service_module_name, ?MODULE ).

-define( service_registration_name, ?service_module_name ).


-export([ % Base service behaviour:

		  % Auto-exported by the Seaplus parse transform:
		  % start/0, start_link/0, stop/0,

		  restart/0,
		  activate_seaplus/1 ]).



% The following start/0, start_link/0 and stop/0 functions are commented-out, as
% they are automatically defined appropriately by the Seaplus parse transform
% now.


% Starts the service.
%
% Preferred form, to detect crashes of the driver and/or of the integrated
% library: the current process will then trap EXIT signals, and thus Seaplus
% will throw a { driver_crashed, ErrorReason } exception if appropriate.
%
% As a result, the user code may catch this exception and react accordingly (ex:
% by restaring the driver).
%
%-spec start() -> void().
%start() ->
	% Allows to register and identify target executable:
%	seaplus:start( ?service_module_name ).



% Starts and links the service.
%
% The current process will then not trap EXIT signals, and thus will crash
% whenever its associated driver crashed (because of the driver itself, or
% because of the integrated library).
%
% Note that start/0 is the generally preferred form.
%
%-spec start_link() -> void().
%start_link() ->
	% Allows to register and identify target executable:
%	seaplus:start_link( ?service_module_name ).



% Restarts the support of this foobar service (ex: to overcome a detected crash
% thereof).
%
% Note: performs a start/0, not a start_link/0.
%
-spec restart() -> void().
restart() ->
	% So that user-code (not Seaplus one only) is triggered as well:
	stop(),
	start().



% Stops the service.
%-spec stop() -> void().
%stop() ->
%	seaplus:stop( ?service_module_name ).



% None defined yet:
-type seaplus_option() :: any().



-ifndef(override_seaplus_activation).


% Simply defining that function allows to activate the Seaplus transformations
% for the current module (acts as a AST marker, and will be detected and removed
% by the Seaplus parse transform).
%
-spec activate_seaplus( [ seaplus_option() ] ) -> void().
activate_seaplus( Options ) ->
	Options.


-endif. % override_seaplus_activation
