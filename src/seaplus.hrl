% This header file auto-defines base, generic service functions.

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
