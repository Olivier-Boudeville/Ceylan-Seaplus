% Copyright (C) 2018-2021 Olivier Boudeville
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
% Creation date: December 16, 2018.



% The main Seaplus module gathers the generic elements useful to integrate any
% kind of C-based service to Erlang.
%
% Relies (only) on Ceylan-Myriad.
%
-module(seaplus).



% Design notes:


% Examples like http://erlang.org/doc/tutorial/erl_interface.html#erlang-program
% show a design where an intermediate process is holding the port.
%
% We chose to hold the port reference directly in the user process, for more
% direct communications.


% We have to rely on a side-effect means of getting that port, as user call
% shall be as lean and mean as possible (no user-level context to keep).
%
% This could be done each thanks to a look-up in the (ex: local) name registery,
% or through a value put in the process dictionary.
%
% We went for the latter, as it is a per-process setting, as opposed to a
% per-node one, allowing for more flexibility in the number of instances
% (i.e. some services might be instantiated more than once, for example with one
% instance of them on as many user processes as needed).
%
% In all cases, Seaplus shall not interfere with any other convention (hence a
% seaplus-specific key being defined), and should allow the user code (more
% precisely: each user process) to make use of multiple Seaplus-based services
% (i.e. a service foo and a service bar from a given user process, regardless of
% the multiplicity of each of them).


% Implementation notes:
%
% Seaplus allows, from a single process, to interact with multiple
% (Seaplus-based) services.
%
% This service support depends, beyond Erlang, (only) on Ceylan-Myriad.
%
% The caller process exchanges directly with the Erlang port Seaplus spawned on
% its behalf, knowing that selective receive already allows to perform
% multiplexed, asynchronous calls and result gathering.
%
% As for the port itself, it is to interact directly (through pipe-like
% channels) with the (C-based) driver executable making the service of interest
% available to Erlang.
%
% The port referebce is stored in the process dictionary of the user process,
% for easier, more transparent management.




-export([ start/1, start_link/1, start/2, start_link/2,
		  restart/1, restart/2, stop/1,
		  call_port_for/3, get_execution_target/0,
		  check_driver_runnable/2, display_driver_runtime_info/2 ]).


% The name of a C-based service to make available:
-type service_name() :: atom().


% A key corresponding to the port of a service instance, whose reference is to
% be stored in the process dictionary of the user process:
%
-type service_key() :: process_dictionary:key().


% The identifier of a function for the driver, as determined by Seaplus:
%
% (ex: 1 for foo/1 in the toy example)
%
-type function_driver_id() :: basic_utils:count().


% The list of parameters to call the function stub with:
-type function_params() :: [ term() ].


% The (Erlang-side) result of the execution of a function:
-type function_result() :: term().


-export_type([ function_driver_id/0, function_params/0, function_result/0 ]).


-define( service_port_key_prefix, "_seaplus_port_for_service_" ).


% To define get_execution_target/0:
-include_lib("myriad/include/utils/basic_utils.hrl").



% Shorthands:

-type ustring() :: text_utils:ustring().

-type executable_name() :: file_utils:executable_name().
-type executable_path() :: file_utils:executable_path().



% Thanks to the service_integration parse transform, a right, minimal, optimal
% API is automatically generated.
%
% More precisely, from the type specifications of the API functions, the Seaplus
% transform automatically generates:
% - their export: -export([ foo/1, bar/2, baz/2, tur/0, frob/1 ]).
% - their definition, like in:
%
% foo( A ) ->
%   seaplus:call_port_for( ?foobar_port_dict_key, 1, [ A ] ).
%
% bar( A, B ) ->
%   seaplus:call_port_for( ?foobar_port_dict_key, 2, [ A, B ] ).
%
% where for example 1 is the static index (compile-time, immediate value) chosen
% for foo/1 (and foobar_port_dict_key is the service-specific key of foobar in
% the user process dictionary, so that multiple Seaplus-using services can
% coexist)
%
% - the function identifier mapping, made available thanks to a
% 'foobar_seaplus_api_mapping.h' generated C header file (to be included in
% 'foobar_seaplus_driver.c') containing for example:
%
% """
%/*
% * Of course these identifiers must match their Erlang counterparts:
% *
% * ('const fun_id foo_1_id = 1 ;' could not be used with switch...)
% *
% */
%#define FOO_1_ID  1
%#define BAR_2_ID  2
%#define BAZ_2_ID  3
%#define TUR_0_ID  4
%#define FROB_1_ID 5
%
% """
%
% (format: (FUNCTION_NAME)_(ARITY)_ID; arity is specified as of course, on both
% sides, two different functions might bear the same name but then should have
% different arities)
%
% - all relevant utility functions transverse to all services (start/0, etc.)
%
% As a result, the service-specific Erlang part is minimal (foobar.erl mostly
% contains the type specifications of the API functions), while the C part only
% has to take care of the service-specific remaining work: demarshalling
% arguments (Erlang to C), executing the corresponding C function based on them,
% marshalling the corresponding results (C to Erlang), like in:
%
% [...]
% case FOO_1_ID:
%	  // Second one is its (single, int) parameter:
%	  write_as_int( buffer, paramTuple, foo( get_as_int( 2, paramTuple ) ) ) ;
%	  break ;
% [...]
%
% That'it! Thanks to these elements, user (Erlang) code shall be able to use the
% foobar service, for example:
%
% [...]
% Res = foobar:foo( 42 ),
% [...]




% Starts the support for the specified named service.
%
% The corresponding executable driver is implicit here, so its name is expected
% to be the one of the service once suffixed with "_seaplus_driver".
%
% For example, a service 'foobar', hence having the Erlang-side bridge
% implemented in foobar.erl, is expected here to rely on the
% 'foobar_seaplus_driver' generated executable.
%
% Note: as the created port is not linked here, as a side-effect the caller
% (user) process will be set to trapping exit signals, so that EXIT messages can
% be received, and be translated to exceptions to be raised. This is the
% recommended choice.
%
-spec start( service_name() ) -> void().
start( ServiceName ) when is_atom( ServiceName ) ->

	% Not supplied here, hence expected to match the service name:
	DriverExecName = get_driver_name( ServiceName ),

	start( ServiceName, DriverExecName ).



% Starts and links the support for the specified named service.
%
% The corresponding driver is implicit here, so its name is expected to be the
% one of the service once suffixed with "_seaplus_driver".
%
% For example, a service 'foobar', hence having the Erlang-side bridge
% implemented in foobar.erl, is expected here to rely on the
% 'foobar_seaplus_driver' generated executable.
%
% Note: as the created port is linked here, as a side-effect the caller (user)
% process will be set to *not* trapping exit signals; so it will die whenever a
% port-side problem happens. This is not the recommended choice, prefer start/1.
%
-spec start_link( service_name() ) -> void().
start_link( ServiceName ) when is_atom( ServiceName ) ->

	% Not supplied here, hence expected to match the service name:
	DriverExecName = get_driver_name( ServiceName ),

	start_link( ServiceName, DriverExecName ).



% Starts the support for the specified named service, relying on specified
% executable name for the driver.
%
% Note: should the service itself or its driver crash (ex: in the context of a
% call being triggered), the service user process will receive an
% {'EXIT',FromPort,Reason} message.
%
-spec start( service_name(), executable_name() ) -> void().
start( ServiceName, DriverExecutableName )
  when is_atom( ServiceName ) andalso is_list( DriverExecutableName ) ->

	DriverExecPath = get_driver_path( ServiceName, DriverExecutableName ),

	launch( ServiceName, DriverExecPath ).



% Starts and links to the caller the support for the specified named service,
% relying on specified executable name for the driver.
%
% Note: should the service itself or its driver crash (ex: in the context of a
% call being triggered), the service user process will receive an exit signal
% with an exit reason other than normal.
%
-spec start_link( service_name(), executable_name() ) -> void().
start_link( ServiceName, DriverExecutableName )
  when is_atom( ServiceName ) andalso is_list( DriverExecutableName ) ->

	DriverExecPath = get_driver_path( ServiceName, DriverExecutableName ),

	launch_link( ServiceName, DriverExecPath ).


% Restarts the specific service support (ex: to overcome a detected crash
% thereof).
%
-spec restart( service_name() ) -> void().
restart( ServiceName ) ->
	stop( ServiceName ),
	start( ServiceName ).


% Restarts the specific service support (ex: to overcome a detected crash
% thereof).
%
-spec restart( service_name(), executable_name() ) -> void().
restart( ServiceName, DriverExecutableName ) ->
	stop( ServiceName ),
	start( ServiceName, DriverExecutableName ).


% Stops the specific service support.
-spec stop( service_name() ) -> void().
stop( ServiceName ) when is_atom( ServiceName ) ->

	cond_utils:if_defined( seaplus_debug_general, trace_bridge:debug_fmt(
		"Stopping the '~s' service.", [ ServiceName ] ) ),

	ServiceKey = get_service_port_key_for( ServiceName ),

	case process_dictionary:get( ServiceKey ) of

		undefined ->
			trace_bridge:warning_fmt( "Service key '~s', for service '~s', "
				"not found, so service is supposed not to be running - "
				"hence not to be stopped.", [ ServiceKey, ServiceName ] ),
			ok;

		TargetPort ->
			%trace_bridge:debug( "Stopping Seaplus." ),
			process_dictionary:remove( ServiceKey ),
			TargetPort ! { self(), close },

			receive

				{ TargetPort, closed } ->
					cond_utils:if_defined( seaplus_debug_port,
						trace_bridge:debug_fmt( "Port ~w stopped.",
												[ TargetPort ] ) ),
					ok

			after 5000 ->
					trace_bridge:error_fmt( "Time-out after waiting for the "
											"stop of port ~w.", [ TargetPort ] )

		end

	end.



% Helper section.


% Returns the filename of the executable corresponding to specified service.
%
% (helper)
%
-spec get_driver_name( service_name() ) -> executable_name().
get_driver_name( ServiceName ) ->
	text_utils:format( "~s_seaplus_driver", [ ServiceName ] ).



% Returns the path to the executable corresponding to specified service.
%
% (helper)
%
-spec get_driver_path( service_name(), executable_name() ) -> executable_path().
get_driver_path( ServiceName, DriverExecutableName ) ->

	% Current directory may not be in user PATH:
	ExecPath = case executable_utils:lookup_executable(
					DriverExecutableName, [ "." ] ) of

		false ->
			PathStr = case system_utils:get_environment_variable( "PATH" ) of

				false ->
					"no PATH environment variable being set";

				PathValue ->
					text_utils:format( "the PATH environment variable being "
						"set to '.:~s'", [ PathValue ] )

			end,

			trace_bridge:error_fmt( "Unable to find executable '~s' "
				"for service '~s' from '~s' (~s).",
				[ DriverExecutableName, ServiceName,
				  file_utils:get_current_directory(), PathStr ] ),

			throw( { executable_not_found, DriverExecutableName,
					 ServiceName } );

		Path ->
			Path

	end,

	%trace_bridge:debug_fmt( "Initializing service '~s', "
	%     "using executable '~s'.", [ ServiceName, ExecPath ] ),

	ExecPath.



% Launches specified service support.
%
% DriverExecPath supposed already checked for existence.
%
% (helper)
%
-spec launch( service_name(), executable_path() ) -> void().
launch( ServiceName, DriverExecPath ) ->

	cond_utils:if_defined( seaplus_debug_general, trace_bridge:debug_fmt(
		"[~w] Launching the '~s' service, using driver path '~s'.",
		[ self(), ServiceName, DriverExecPath ] ) ),

	% To receive EXIT messages, should the port fail (best option):
	process_flag( trap_exit, true ),

	% No need to create a process_in-the-middle:
	%spawn( fun() -> init_driver( ServiceName, DriverExecPath ) end ),
	init_driver( ServiceName, DriverExecPath ).


% (helper)
launch_link( ServiceName, DriverExecPath ) ->

	% To be killed in turn should the port fail (not the best option):
	process_flag( trap_exit, false ),

	% No need to create a process-in-the-middle:
	%spawn_link( fun() -> init_driver( ServiceName, DriverExecPath ) end ),
	init_driver( ServiceName, DriverExecPath ).



% Inits the driver of specified service.
%
% DriverExecPath supposed already checked for existence.
%
% (helper)
%
init_driver( ServiceName, DriverExecPath ) ->

	cond_utils:if_defined( seaplus_debug_driver, trace_bridge:debug_fmt(
		"For service '~s', launching driver '~s'.",
		[ ServiceName, DriverExecPath ] ) ),

	% Used to intercept driver crashes, when was a spawned process:
	%process_flag( trap_exit, true ),

	% Now relying on the process dictionary:
	%trace_bridge:debug_fmt( "Registering (locally) as '~s'.",
	%    [ ServiceName ] ),

	% Not using anymore an intermediate process:
	%naming_utils:register_as( _Pid=self(), _RegistrationName=ServiceName,
	%						  local_only ),

	% Will store the spawned port for later use in the process dictionary of the
	% calling user process:

	ServiceKey = get_service_port_key_for( ServiceName ),

	case process_dictionary:get( ServiceKey ) of

		undefined ->
			ok;

		_ ->
			trace_bridge:error_fmt( "Service key '~s', for service '~s', "
				"already registered; service already started?",
				[ ServiceKey, ServiceName ] ),
			throw( { service_key_already_set, ServiceKey } )

	end,

	% Uncomment if wanting to force the selection of, typically, a library you
	% specifically built with debug symbols, like for example:
	% LibDebugPath = "/home/stallone/Software/libgammu/lib",

	%LibPath = "LD_LIBRARY_PATH",
	%BaseEnv = system_utils:get_environment_variable( LibPath ),

	%NewPathEnv = text_utils:format( "~s:~s", [ LibDebugPath, BaseEnv ] ),
	%ExtraEnv = [ { LibPath, NewPathEnv } ],

	ExtraEnv = [],

	%trace_bridge:debug_fmt( "Extra environment for driver: ~p.",
	%                        [ ExtraEnv ] ),

	% To help any driver-level debugging; notably to just *display* whether
	% libseaplus-*.so will be found:
	%
%	cond_utils:if_defined( seaplus_debug_driver,
%		display_driver_runtime_info( DriverExecPath, ExtraEnv ) ),


	% To perfom an actual check; user code might do the same unconditionally:
	cond_utils:if_defined( seaplus_check_driver,
		case check_driver_runnable( DriverExecPath, ExtraEnv ) of

			{ ErrorCode, ErrorMsg } ->
				trace_bridge:error_fmt( "Driver check failed "
					"(error code ~B; extra environment: ~p): '~s'.",
					[ ErrorCode, ExtraEnv, ErrorMsg ] ),
				display_driver_runtime_info( DriverExecPath, ExtraEnv );

			DriverNormalMessage ->
				trace_bridge:debug_fmt( "Driver check successful "
					"(for extra environment: ~p); returned '~s'.",
					[ ExtraEnv, DriverNormalMessage ] )

		end ),

	PortOptions = [ { packet, 2 }, binary, { env, ExtraEnv } ],

	% If wanting a direct execution of the driver:
	DriverCommand = DriverExecPath,

	% If wanting to run the driver through Valgrind instead:
	%DriverCommand = text_utils:format(
	%	"valgrind --log-file=/tmp/seaplus-valgrind.log ~s",
	%	[ DriverExecPath ] ),


	cond_utils:if_defined( seaplus_debug_driver, trace_bridge:debug_fmt(
		"DriverCommand: '~s'.", [ DriverCommand ] ) ),

	% Respect the erl_interface conventions:
	%
	% (running '"gdb -batch -ex run " ++ DriverExecPath' will not help):
	%
	Port = open_port( { spawn, DriverCommand }, PortOptions ),

	cond_utils:if_defined( seaplus_debug_port, trace_bridge:debug_fmt(
		"Storing port ~w under the service key '~s' "
		"in the process dictionary of ~p.", [ Port, ServiceKey, self() ] ) ),

	process_dictionary:put( ServiceKey, Port ),

	% Useful for a post-crash analysis:
	ServiceDriverKey = get_service_driver_key_for( ServiceName ),
	process_dictionary:put( ServiceDriverKey, { DriverExecPath, ExtraEnv } ).

	% No need for a main loop, *we* drive the (direct) communication:
	%driver_main_loop( Port, ServiceName ).



% Checks whether the driver for the specified service is runnable at all.
%
% Defined as a separate function so that user code can anticipate this checking,
% for example when it starts up.
%
-spec check_driver_runnable( executable_path(), system_utils:environment() ) ->
			ustring() | system_utils:execution_outcome().
check_driver_runnable( DriverExecPath, ExtraEnvironment ) ->

	% Allows to pre-check whether the driver can be run at all:
	% (actually any command-line option will do)
	%
	Cmd = DriverExecPath ++ " --help",

	% For the relevance of this check, we perform this test in the exact same
	% environment that will be used just afterwards to run the actual port:
	%
	case system_utils:run_executable( Cmd, ExtraEnvironment ) of

		{ _ExitCode=0, DriverNormalMessage } ->
			DriverNormalMessage;

		Outcome -> %{ ErrorCode, ErrorMsg } ->
			Outcome

	end.



% Displays runtime information about the specified driver, to help
% troubleshooting.
%
-spec display_driver_runtime_info( executable_path(),
								   system_utils:environment() ) -> void().
display_driver_runtime_info( ExecPath, ExtraEnvironment ) ->

	% At least as clear as 'readelf -d XXX':
	LddPath = executable_utils:find_executable( "ldd" ),

	Cmd = text_utils:format( "~s ~s", [ LddPath, ExecPath ] ),

	case system_utils:run_executable( Cmd, ExtraEnvironment ) of

		{ _RetCode=0, CmdOutput } ->
			trace_bridge:info_fmt( "Library dependencies for '~s' are:~n~s~n"
				"While being in '~s':~n  PATH is '~s'~n  "
				"LD_LIBRARY_PATH is '~s' (with extra environment ~p).",
				[ ExecPath, CmdOutput, file_utils:get_current_directory(),
				  system_utils:get_environment_variable( "PATH" ),
				  system_utils:get_environment_variable( "LD_LIBRARY_PATH" ),
				  ExtraEnvironment ] );

		{ RetCode, CmdOutput } ->
			trace_bridge:error_fmt( "Unable to get library dependencies for "
				"'~s' (exit code ~B; with extra environment ~p): '~s'.",
				[ ExecPath, RetCode, ExtraEnvironment, CmdOutput ] )

	end.



% Service Driver section.


% The actual bridge from the user code to the port (and then to the driver).
%
% The identifier will suffice, no real need to pass along the
% basic_utils:function_name().
%
% Will return the result of the corresponding call, or will raise an exception.
%
-spec call_port_for( service_key(), function_driver_id(), function_params() ) ->
						function_result().
call_port_for( ServiceKey, FunctionId, Params ) ->

	TargetPort = case process_dictionary:get( ServiceKey ) of

		undefined ->
			trace_bridge:error_fmt( "Service key '~s' not set in process "
				"dictionary of ~p; has the corresponding service been started?",
				[ ServiceKey, self() ] ),

			throw( { service_key_not_set, ServiceKey } );

		V ->
			V

	end,

	% Vaguely similar to WOOPER conventions (tuple vs list):
	Message = { FunctionId, Params },

	BinMessage = term_to_binary( Message ),

	%trace_bridge:debug_fmt( "Sending command message '~p' (size: ~B bytes) "
	%	"to port ~w.", [ Message, size( BinMessage ), TargetPort ] ),

	% To be handled by the (C-based) driver:
	%
	% (note that message structure and content are dictated by how Erlang ports
	% have been defined; for example 'TargetPort ! { executeFunction, Message,
	% self() }' would not be relevant here, see
	% http://erlang.org/doc/tutorial/c_port.html for more information)
	%
	% Message already encoded as wanted here:
	%
	TargetPort ! { self(), { command, BinMessage } },

	% In case of crash, we remove the service key so that for example any
	% restart triggered by the corresponding exception being caught will not
	% have its stop/0 wait for the driver time-out to expire:

	receive

		% Normal case, receiving the corresponding result:
		{ TargetPort, { data, BinAnswer } } ->

			cond_utils:if_defined( seaplus_debug_driver, trace_bridge:debug_fmt(
				"Term received by ~w from C side "
				"(port: ~w) for service '~s', in answer to "
				"a call to the function whose identifier is ~B:~n~p",
				[ self(), TargetPort,
				  get_service_name_from_port_key( ServiceKey ), FunctionId,
				  BinAnswer ] ) ),

			% If raises {badarg,[{erlang,binary_to_term,[... then probably that
			% this driver performed an incorrect write_*_result for that
			% function:
			%
			try

				binary_to_term( BinAnswer )

			catch

				_:badarg ->
					ServiceName = get_service_name_from_port_key( ServiceKey ),
					trace_bridge:error_fmt( "Incorrect driver return received "
						"by process ~w from port ~w for service '~s' regarding "
						"function identified by ~B:~n~p",
						[ self(), TargetPort, ServiceName, FunctionId,
						  BinAnswer ] ),
					throw( { incorrect_return, { service, ServiceName },
							 { function_id, FunctionId }, BinAnswer } );

				_:E ->
					ServiceName = get_service_name_from_port_key( ServiceKey ),
					trace_bridge:error_fmt( "Exception raised for driver "
						"return received by process ~w from port ~w "
						"for service '~s' regarding the function identified "
						"by ~B: ~p",
						[ self(), TargetPort, ServiceName, FunctionId, E ] ),
					throw( { invalid_return, { service, ServiceName },
							 { function_id, FunctionId }, E } )

			end;

		{ 'EXIT', TargetPort, _Reason=normal } ->

			process_dictionary:remove( ServiceKey ),

			% Actually even when hard crashing (zero division), a 'normal'
			% reason is thrown:
			%
			%trace_bridge:warning_fmt( "Normal EXIT of port ~p.",
			%						 [ TargetPort ] ),

			% In order to call display_driver_runtime_info
			trace_bridge:error_fmt( "Crash of the driver port (~w) reported "
				"to calling process ~w (no reason was specified).",
				[ TargetPort, self() ] ),

			% Fetch driver path:
			DrivKey = get_service_driver_key_from_port_one( ServiceKey ),

			case process_dictionary:get( DrivKey ) of

				undefined ->
					trace_bridge:error_fmt( "Unable to find driver key '~s' "
						"on ~w (abnormal).", [ DrivKey, self() ] );

				{ ExecPath, ExtraEnv } ->
					display_driver_runtime_info( ExecPath, ExtraEnv )

			end,

			throw( { driver_crashed, unknown_reason } );


		% This is not one of our ports; it must be an unrelated operation we
		% should not even interfere with by receiving such message:
		%
		%{ 'EXIT', OtherPort, _Reason=normal } ->
		%  Not our business


		{ 'EXIT', TargetPort, Reason } ->

			process_dictionary:remove( ServiceKey ),

			trace_bridge:error_fmt( "Received exit failure from the driver "
				"port (~p), reason: ~p", [ TargetPort, Reason ] ),

			throw( { driver_crashed, Reason } )

		% Not our business either:
		%{ 'EXIT', OtherPort, Reason } ->

		% No promiscuous mode, we have not to hijack the traffic of others:
		%Unexpected ->
		%	trace_bridge:error_fmt( "Driver call: unexpected message "
		%		"received: ~p~n", [ Unexpected ] ),
		%	throw( { unexpected_driver_message, Unexpected } )

	end.



% Returns the key that shall be used to store information in the process
% dictionary of the calling user process for the specified service.
%
% Note: must agree with seaplus_parse_transform:get_port_dict_key_for/1.
%
-spec get_service_port_key_for( service_name() ) -> service_key().
get_service_port_key_for( ServiceName ) ->

	PortKeyString = text_utils:format( "~s~s",
								   [ ?service_port_key_prefix, ServiceName ] ),

	text_utils:string_to_atom( PortKeyString ).


% Reciprocal of get_service_port_key_for/1.
-spec get_service_name_from_port_key( service_key() ) -> service_name().
get_service_name_from_port_key( ServicePortKey ) ->

	case text_utils:split_after_prefix( ?service_port_key_prefix,
							text_utils:atom_to_string( ServicePortKey ) ) of

		no_prefix ->
			throw( { no_service_prefix, ?service_port_key_prefix,
					 ServicePortKey } );

		Str ->
			text_utils:string_to_atom( Str )

	end.


% Returns the key that shall be used to store the executable path of the driver
% for the specified service.
%
% Note: fully optional, only interest is to help troubleshooting.
%
-spec get_service_driver_key_for( service_name() ) -> service_key().
get_service_driver_key_for( ServiceName ) ->

	DriverKeyString = text_utils:format( "_seaplus_driver_path_for_service_~s",
										 [ ServiceName ] ),

	text_utils:string_to_atom( DriverKeyString ).



% Returns the key that shall be used to store the executable path of the driver
% for the specified service, based on the key for service port.
%
% Note: necessary to deduce one from another as Seaplus is mostly stateless.
%
-spec get_service_driver_key_from_port_one( service_key() ) -> service_key().
get_service_driver_key_from_port_one( ServicePortKey ) ->

	ServiceName = get_service_name_from_port_key( ServicePortKey ),

	% Is actually a string, not a problem though:
	get_service_driver_key_for( ServiceName ).
