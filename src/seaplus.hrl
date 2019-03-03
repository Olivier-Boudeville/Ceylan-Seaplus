% This header file auto-defines base, generic service functions.
%
% It is meant to be included by the bridgin modules (ex: by foobar.erl for a
% Foobar service).


-define( service_module_name, ?MODULE ).

-define( service_registration_name, ?service_module_name ).


-export([ % Base service behaviour:
		  start/0, start_link/0,  restart/0, stop/0, activate_seaplus/1,
		  get_service_key/0 ]).


% Starts the service.
%
% Note: not linking caller here; so, if the C code crashes, the corresponding
% call will block.
%
start() ->
	% Allows to register and identify target executable:
	seaplus:start( ?service_module_name ).



% Starts and links the service.
%
% Preferred form, to detect crashes.
%
start_link() ->
	% Allows to register and identify target executable:
	seaplus:start_link( ?service_module_name ).


% Restarts the supportof this foobar service (ex: to overcome a detected crash
% thereof).
%
-spec restart() -> void().
restart() ->
	seaplus:restart( ?MODULE ).



% Stops the service.
%
stop() ->
	seaplus:stop( ?service_module_name ).



% None defined yet:
-type seaplus_option() :: any().



-ifndef(override_seaplus_activation).


% Simply defining that function allows to activate the Seaplus transformations
% for the current module.
%
-spec activate_seaplus( [ seaplus_option() ] ) -> void().
activate_seaplus( Options ) ->
	Options.


-endif. % override_seaplus_activation
