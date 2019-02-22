% This header file auto-defines base, generic service functions.
%
% It is meant to be included by the bridgin modules (ex: by foobar.erl for a
% Foobar service).


-define( service_module_name, ?MODULE ).

-define( service_registration_name, ?service_module_name ).


-export([ % Base service behaviour:
		  start/0, start_link/0, stop/0,

		  % Service-related meta-data:
		  list_exported_service_functions/0 ]).


% Starts the service.
%
% Note: not linking caller here; so, if the C code crashes, the corresponding
% call will block.
%
start() ->
	% Allows to register and identify target executable:
	service_support:start( ?service_module_name ).



% Starts and links the service.
%
% Preferred form, to detect crashes.
%
start_link() ->
	% Allows to register and identify target executable:
	service_support:start_link( ?service_module_name ).


% Stops the service.
%
stop() ->
	service_support:stop( ?service_module_name ).



% None defined yet:
-type seaplus_option() :: any().



-ifndef(override_seaplus_activation).


% Simply defining that function allows to activate the Seaplus transformations
% for the current module.
%
-spdec activate_seaplus( [ seaplus_option() ] ) -> void().
activate_seaplus( Options ) ->
	Options.


-endif. % override_seaplus_activation

