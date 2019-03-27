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
% Creation date: Tuesday, January 29, 2019



% Overall parse transform for the Seaplus layer, in charge of streamlining the
% integration of any C-based service.
%
% Meant, for a Foobar service, to operate on a foobar.erl stub, so that:
%
% - a fully-functional foobar module becomes available
%
% - a corresponding foobar_seaplus_api_mapping.h C header is generated in order
% to ease the development of the corresponding C-side driver
%
-module(seaplus_parse_transform).



% Implementation notes:

% Calls in turn the Myriad parse transform, before and after the Seaplus-level
% operations have been completed (respectively to obtain a module_info as input
% for Seaplus, and to transform adequately, as standard Erlang code, any
% Seaplus-injected code that would rely on Myriad conventions).
%
% One will get: 'undefined parse transform 'seaplus_parse_transform'' as soon as
% a compiled module called by the parse transform (ex: text_utils.beam) will not
% be found (hence even if the transform itself is available) or a non-exported
% (or even not existing) function is called (ex: text_utils:format/1).


% Regarding the Seaplus parse transform.
%
% This parse transform will largely transform and enrich a module stub
% corresponding to a service.
%
% However, multiple modules exist in the Seaplus layer, some of which must
% undergo such a transformation, some others not (ex: they are just helper
% modules).
%
% To discriminate between these two sets, we do not rely on the module name as
% we do not want to constrain the name of the bridging module (ex: 'foobar' is
% fine, we do not want to make a longer form such as 'foobar_service'
% compulsory).
%
% So the trigger of the actual Seaplus transformations will be the definition of
% a specific function, activate_seaplus/1, a default implementation of which
% being defined in seaplus.hrl.
%
% As a result, including that header will imply that the module at hand is a
% Seaplus stub.



-export([ run_standalone/1, run_standalone/2,
		  parse_transform/2, apply_seaplus_transform/2 ]).



% For function_info:
-include("ast_info.hrl").


% For ast_transforms undefined record:
-include("ast_transform.hrl").

-type dict_key() :: atom().


% Local shorthands:

-type ast() :: ast_base:ast().
%-type location() :: ast_base:form_location().
-type module_info() :: ast_info:module_info().
-type function_info() :: ast_info:function_info().
-type ast_transforms() :: ast_transform:ast_transforms().

-type function_driver_id() :: seaplus:function_driver_id().



-ifdef(enable_seaplus_traces).

-define( display_trace( S ), trace_utils:trace( "[Seaplus] " ++ S ) ).

-define( display_trace( S, F ),
		 ast_utils:trace_fmt( "[Seaplus] " ++ S, F ) ).

-else. % enable_seaplus_traces

% To avoid variables being reported as unused depending on the mode:

-define( display_trace( S ),
		 basic_utils:ignore_unused( { seaplus_trace_disabled, S } ) ).

-define( display_trace( S, F ),
		 basic_utils:ignore_unused({ seaplus_trace_disabled, S, F } ) ).

-endif. % enable_seaplus_traces





% Implementation notes:

% For log output, even if io:format/{1,2} and ast_utils:display_*/* work, we
% recommend using trace_utils:*/*.



% Runs the Seaplus parse transform defined here in a standalone way (i.e. without
% being triggered by the usual, integrated compilation process), with no
% specific preprocessor option.
%
% This allows to benefit from all compilation error and warning messages,
% whereas they are seldom available from a code directly run as a parse
% transform (ex: 'undefined parse transform 'foobar'' as soon as a function or a
% module is not found).
%
-spec run_standalone( file_utils:file_name() ) -> { ast(), module_info() }.
run_standalone( FileToTransform ) ->
	run_standalone( FileToTransform, _PreprocessorOptions=[] ).



% Runs the Seaplus parse transform defined here in a standalone way (i.e. without
% being triggered by the usual, integrated compilation process), with specified
% preprocessor options.
%
% This allows to benefit from all compilation error and warning messages,
% whereas they are seldom available from a code directly run as a parse
% transform (ex: 'undefined parse transform 'foobar'' as soon as a function or a
% module is not found).
%
-spec run_standalone( file_utils:file_name(),
			  [ ast_utils:preprocessor_option() ] ) -> { ast(), module_info() }.
run_standalone( FileToTransform, PreprocessorOptions ) ->

	InputAST = ast_utils:erl_to_ast( FileToTransform, PreprocessorOptions ),

	% Necessary to fetch resources:
	SeaplusRootDir = get_seaplus_root( PreprocessorOptions ),

	% Returns { SeaplusAST, ModuleInfo }:
	apply_seaplus_transform( InputAST, SeaplusRootDir ).



% The parse transform itself, generating notably (Myriad-based) Abstract Format
% code, before being itself converted in turn into an Erlang-compliant Abstract
% Format code.
%
-spec parse_transform( ast(), list() ) -> ast().
parse_transform( InputAST, Options ) ->

	%trace_utils:trace_fmt( "Seaplus input AST:~n~p~n", [ InputAST ] ),

	%trace_utils:trace_fmt( "Seaplus options:~n~p~n", [ Options ] ),

	% Necessary to fetch resources:
	SeaplusRootDir = get_seaplus_root( Options ),

	%ast_utils:write_ast_to_file( InputAST, "Seaplus-input-AST.txt" ),

	% In the context of this direct parse transform, the module_info is of no
	% use afterwards and thus can be dropped:
	%
	{ SeaplusAST, _ModuleInfo } =
		apply_seaplus_transform( InputAST, SeaplusRootDir ),

	%trace_utils:trace_fmt( "Seaplus output AST:~n~p~n", [ SeaplusAST ] ),

	%ast_utils:write_ast_to_file( SeaplusAST, "Seaplus-output-AST.txt" ),

	SeaplusAST.


% Returns the root directory of Seaplus.
get_seaplus_root( Options ) ->

	case [ RootDir || { d, 'SEAPLUS_ROOT', RootDir } <- Options ] of

		[ RootDirectory ] ->

			case file_utils:is_existing_directory( RootDirectory ) of

				true ->
					%trace_utils:debug_fmt( "Seaplus directory is '~s'.",
					%					   [ RootDirectory ] ),
					RootDirectory;

				false ->
					trace_utils:error_fmt(
					  "Seaplus directory '~s' does not exist.",
					  [ RootDirectory ] ),
					throw( { seaplus_directory_not_found, RootDirectory } )

			end;

		[] ->
			trace_utils:error( "Seaplus directory not set in build." ),
			throw( seaplus_directory_not_set );

		Others ->
			trace_utils:error( "Multiple Seaplus directories set: ~p.",
							   [ Others ] ),
			throw( multiple_seaplus_directories )

	end.





% Transforms specified AST for Seaplus.
-spec apply_seaplus_transform( ast(), file_utils:directory_path() ) ->
									 { ast(), module_info() }.
apply_seaplus_transform( InputAST, SeaplusRootDir ) ->

	%trace_utils:debug_fmt( "  (applying parse transform '~p')", [ ?MODULE ] ),

	%trace_utils:debug_fmt( "~n## INPUT ####################################" ),
	%trace_utils:debug_fmt( "Seaplus input AST:~n~p~n~n", [ InputAST ] ),

	%ast_utils:write_ast_to_file( InputAST, "Seaplus-input-AST.txt" ),

	% This allows to compare input and output ASTs more easily:
	%ast_utils:write_ast_to_file( lists:sort( InputAST ),
	%							 "Seaplus-input-AST-sorted.txt" ),

	% First preprocesses the AST based on the Myriad parse transform, in order
	% to benefit from its corresponding module_info record:
	% (however no Myriad-level transformation performed yet)
	%
	InputModuleInfo = ast_info:extract_module_info_from_ast( InputAST ),

	?display_trace( "Module information extracted." ),

	%ast_utils:display_debug( "Module information, directly as obtained "
	%				"from Myriad (untransformed): ~s",
	%				[ ast_info:module_info_to_string( InputModuleInfo ) ] ),

	% The Seaplus augmentations must be applied only to modules corresponding to
	% services to be integrated (not to all modules):
	%
	ProcessedModuleInfo = case is_integration_module( InputModuleInfo ) of

		false ->
			% Then Seaplus does nothing specific:
			InputModuleInfo;

		ShrunkModuleInfo ->
			% Then promote this Myriad-level information into a Seaplus one:
			% (here is the real Seaplus magic, if any)
			%
			process_module_info_from( ShrunkModuleInfo, SeaplusRootDir )

	end,

	% In all cases, Myriad transformation shall happen (ex: at the very least,
	% we want types like void() to be transformed):
	%
	{ FinalModuleInfo, _MyriadTransforms } =
		myriad_parse_transform:transform_module_info( ProcessedModuleInfo ),


	%trace_utils:debug_fmt(
	%  "Module information after Seaplus: ~s",
	%  [ ast_info:module_info_to_string( FinalModuleInfo ) ] ),

	?display_trace( "Module information processed, "
					"recomposing corresponding AST." ),

	OutputAST = ast_info:recompose_ast_from_module_info(
				  FinalModuleInfo ),

	%trace_utils:debug_fmt( "Seaplus output AST:~n~p", [ OutputAST ] ),

	%OutputASTFilename = text_utils:format(
	%		   "Seaplus-output-AST-for-module-~s.txt",
	%			[ element( 1, FinalModuleInfo#module_info.module ) ] ),
	%
	%ast_utils:write_ast_to_file( OutputAST, OutputASTFilename ),

	%OutputSortedASTFilename = text_utils:format(
	%		   "Seaplus-sorted-output-AST-for-module-~s.txt",
	%			[ element( 1, FinalModuleInfo#module_info.module ) ] ),
	%
	%ast_utils:write_ast_to_file( lists:sort( OutputAST ),
	%							  OutputSortedASTFilename ),

	{ OutputAST, ProcessedModuleInfo }.




% Determines whether specified module info corresponds to a service-integration
% module, i.e. a module that Seaplus shall augment based on the unimplemented
% specs found.
%
-spec is_integration_module( module_info() ) -> 'false' | module_info().
is_integration_module( ModuleInfo=#module_info{ functions=FunctionTable } ) ->

	MarkerFunId = { activate_seaplus, 1 },

	% A module will be a service-integration one iff activate_seaplus/1 has been
	% defined (probably automatically, by including seaplus.hrl), in which case
	% it will be removed:
	%
	case table:extractEntryIfExisting( MarkerFunId, FunctionTable ) of

		false ->
			%trace_utils:debug(
			%  "(not detected as a service-integration module)" ),
			false;

		{ #function_info{ exported=ExportLocs }, ShrunkFunctionTable } ->

			%trace_utils:debug( "(detected as a service-integration module)" ),

			% It must also be un-exported:

			FunExportTable = ModuleInfo#module_info.function_exports,

			ShrunkFunExportTable = ast_info:ensure_function_not_exported(
					MarkerFunId, ExportLocs, FunExportTable ),

			ModuleInfo#module_info{
			  function_exports=ShrunkFunExportTable,
			  functions=ShrunkFunctionTable }

	end.


% Applies the actual Seaplus transformations.
-spec process_module_info_from( module_info(), file_utils:directory_name() ) ->
									  module_info().
process_module_info_from(
  ModuleInfo=#module_info{ module={ ModName, _Loc } }, SeaplusRootDir ) ->

	ReadyFunInfos = prepare_api_functions( ModuleInfo ),

	SelectFunIds = [ { Name, Arity }
			 || #function_info{ name=Name, arity=Arity } <- ReadyFunInfos ],

	FullModuleInfo = case SelectFunIds of

		[] ->
			% We nevertheless may want a (empty) header file to be produced:
			trace_utils:debug( "No API function detected." ),
			ModuleInfo;

		_ ->
			trace_utils:debug_fmt( "Selected ~B function(s) for API: ~s",
				[ length( SelectFunIds ), text_utils:strings_to_string(
					[ ast_info:function_id_to_string( Id )
					  || Id <- SelectFunIds ] ) ] ),

			% Generating the header for the driver:
			HeaderFilename = generate_driver_header( ModName, SelectFunIds ),

			manage_driver_implementation( ModName, SelectFunIds,
										  HeaderFilename, SeaplusRootDir ),

			reinject_fun_infos( ReadyFunInfos, ModuleInfo )

	end,

	% At the very least, we want types like void() to be transformed:
	{ MyriadModuleInfo, _MyriadTransforms } =
		myriad_parse_transform:transform_module_info( FullModuleInfo ),

	MyriadModuleInfo.


% Generates the relevant C header file for the service driver.
generate_driver_header( ServiceModuleName, FunIds ) ->

	HeaderFilename = text_utils:format( "~s_seaplus_api_mapping.h",
										[ ServiceModuleName ] ),

	trace_utils:trace_fmt( "Generating the '~s' header file, comprising ~B "
						   "function mappings.",
						   [ HeaderFilename, length( FunIds ) ] ),

	% Being a generated file, it can be overwritten with no regret:
	HeaderFile = file_utils:open( HeaderFilename, _Opts=[ write, raw ] ),

	StringModName = text_utils:atom_to_string( ServiceModuleName ),

	IncGuard = text_utils:format( "_~s_SEAPLUS_API_MAPPING_H_",
						  [ text_utils:to_uppercase( StringModName ) ] ),

	file_utils:write( HeaderFile, "#ifndef ~s~n", [ IncGuard ] ),
	file_utils:write( HeaderFile, "#define ~s~n~n", [ IncGuard ] ),

	file_utils:write( HeaderFile,
		  "/* This header file has been generated by the Seaplus integration~n"
		  " * bridge for the '~s' service, on ~s.~n"
		  " */~n~n",
		  [ StringModName, time_utils:get_textual_timestamp() ] ),

	file_utils:write( HeaderFile,
					  "/* For each of the exposed functions of the API, "
					  "a Seaplus identifier is~n"
					  " * generated to ensure that the C code of the driver "
					  "can stay in sync with~n"
					  " * the Erlang view on said API, regardless of its changes.~n"
					  " */~n~n", [] ),

	write_mapping( HeaderFile, FunIds, _Count=1 ),

	file_utils:write( HeaderFile, "~n#endif // ~s~n", [ IncGuard ] ),

	file_utils:close( HeaderFile ),

	HeaderFilename.


% (helper)
write_mapping( _HeaderFile, _FunIds=[], _Count ) ->
	ok;

write_mapping( HeaderFile, _FunIds=[ { FunName, Arity } | T ], Count ) ->

	FunSymbol = get_driver_id_for( FunName, Arity ),

	file_utils:write( HeaderFile, "#define ~s ~B~n", [ FunSymbol, Count ] ),

	write_mapping( HeaderFile, T, Count+1 ).



% Returns the C driver identifier for specified function.
get_driver_id_for( FunName, Arity ) ->

	FunString = text_utils:to_uppercase( text_utils:atom_to_string( FunName ) ),

	text_utils:format( "~s_~B_ID", [ FunString, Arity ] ).


% Creates an implementation stub for the driver, if no such file exists.
manage_driver_implementation( ServiceModuleName, FunIds, HeaderFilename,
							  SeaplusRootDir ) ->

	SourceFilename = text_utils:format( "~s_seaplus_driver.c",
										[ ServiceModuleName ] ),

	case file_utils:is_existing_file_or_link( SourceFilename ) of

		true ->
			trace_utils:info_fmt( "Driver implementation ('~s') already "
								  "existing, not generating it.",
								  [ SourceFilename ] );

		false ->
			trace_utils:info_fmt( "No driver implementation ('~s') found, "
								  "generating it.",
								  [ SourceFilename ] ),
			generate_driver_implementation( ServiceModuleName, FunIds,
						HeaderFilename, SourceFilename, SeaplusRootDir )

	end.



% Generates the implementation stub for the driver, overwriting it if needed.
generate_driver_implementation( ServiceModuleName, FunIds, HeaderFilename,
								SourceFilename, SeaplusRootDir ) ->

	TemplateBaseDir = file_utils:join( SeaplusRootDir, "src" ),

	DriverHeaderFilename = file_utils:join( TemplateBaseDir,
											"seaplus_driver_header.c" ),

	case file_utils:is_existing_file( DriverHeaderFilename ) of

		true ->
			ok;

		false ->
			throw( { driver_header_not_found, DriverHeaderFilename } )

	end,

	HeaderContent = file_utils:read_whole( DriverHeaderFilename ),

	%trace_utils:debug_fmt( "Read content:~n~s",
	%					   [ HeaderContent ] ),

	HHeaderContent = string:replace(  HeaderContent,
	   "##SEAPLUS_SERVICE_HEADER_FILE##", HeaderFilename, all ),

	%trace_utils:debug_fmt( "New content:~n~s", [ HHeaderContent ] ),

	StringServiceModuleName = text_utils:atom_to_string( ServiceModuleName ),

	NHeaderContent = string:replace( HHeaderContent,
				   "##SEAPLUS_SERVICE_NAME##", StringServiceModuleName, all ),


	%trace_utils:debug_fmt( "Generated driver header:~n~s",
	%                      [ NHeaderContent ] ),

	DriverFooterFilename = file_utils:join( TemplateBaseDir,
											"seaplus_driver_footer.c" ),

	case file_utils:is_existing_file( DriverFooterFilename ) of

		true ->
			ok;

		false ->
			throw( { driver_footer_not_found, DriverFooterFilename } )

	end,

	FooterContent = file_utils:read_whole( DriverFooterFilename ),

	SourceFile = file_utils:open( SourceFilename, _Opts=[ write, raw ] ),

	file_utils:write( SourceFile, NHeaderContent ),

	write_cases( SourceFile, FunIds ),

	file_utils:write( SourceFile, FooterContent ),

	file_utils:close( SourceFile ).


write_cases( _SourceFile, _FunIds=[] ) ->
	ok;

write_cases( SourceFile, _FunIds=[ { FunName, Arity } | T ] ) ->

	DriverId = get_driver_id_for( FunName, Arity ),

	Snippet = text_utils:format(
		"    case ~s:~n~n"
		"        LOG_DEBUG( \"Executing ~s/~B.\" ) ;~n"
		"        check_arity_is( ~B, param_count, ~s ) ;~n~n"
		"        // Add an Erlang term -> C conversion here for each "
				"parameter of interest:~n"
		"        // (refer to seaplus_getters.h for the conversion functions)~n"
		"        // Ex (supposing int): int i = get_parameter_as_int( "
		"1, parameters ) ;~n~n"

		"        // Add call to the C counterpart of ~s/~B:~n"
		"        // Ex: float f = some_service_function( i ) ;~n~n"
		"        // Write the returned result to buffer:~n"
		"        // (refer to seaplus_setters.h for the conversion functions)~n"
		"        // Ex: write_as_double( buffer, (double) f ) ;~n~n"
		"        // Do not forget to deallocate any relevant memory!~n~n"
		"        break ;~n",
		[ DriverId, FunName, Arity, Arity, DriverId, FunName, Arity ] ),

	file_utils:write( SourceFile, "~n~s~n", [ Snippet ] ),

	write_cases( SourceFile, T ).



% Identifies the API functions, processes and sorts them.
-spec prepare_api_functions( module_info() ) -> [ function_info() ].
prepare_api_functions( ModuleInfo=#module_info{ functions=FunctionTable,
												markers=MarkerTable } ) ->

	% By convention, the API functions are exactly the ones:
	%
	% - not defined by Seaplus (as including seaplus.hrl results, as a side
	% effect, in defining start/0 and others)
	%
	% - and with a spec (so that the user still can opt out a function by not
	% defining a spec for it; then this function will not be part of the Seaplus
	% binding)
	%
	% Such selected functions may or may not be defined; then, respectively,
	% either Seaplus will re-use the already provided implementation once
	% transformed, or generate one from scratch for them.

	% All collected module-level function information:
	AllFunInfos = table:values( FunctionTable ),

	% Seaplus additions (are included in AllFunInfos):
	SeaplusFunIds = get_seaplus_function_ids(),

	% All the functions selected to form the binding API:
	SelectedFunInfos = select_for_binding( AllFunInfos, SeaplusFunIds, _Acc=[] ),

	% We then order the returned function_info records based on the location of
	% their spec (so that their IDs correspond to their in-source order):
	%
	% (we rely on the fact that a function_info is a record whose 6th (i.e. 7
	% minus 1 for the record tag) field is the located spec, which is a pair
	% whose order is by rule determined first by its first element - which is
	% the spec location)
	%
	OrderedSelected = lists:keysort( _Index=7, SelectedFunInfos ),

	% Key in the process dictionary under which the service port will be stored:
	PortDictKey = get_port_dict_key_for( ModuleInfo ),

	%trace_utils:debug_fmt( "Will store the service port under the "
	%   "'~s' key in the process dictionary.", [ PortDictKey ] ),

	MarkerTable = ModuleInfo#module_info.markers,

	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),

	DefLoc = table:getEntry( definition_functions_marker, MarkerTable ),

	% Now that the order is known, we can generate or transform these API
	% functions:
	%
	post_process_fun_infos( OrderedSelected, PortDictKey, ExportLoc, DefLoc ).



% Selects the functions to be included in the binding.
%
% Too early to determine whether they should be generated or transformed
% (i.e. to look at their clauses), we need to number them first.
%
% (helper)
%
select_for_binding( _AllFunInfos=[], _SeaplusFunIds, Acc ) ->
	Acc;

% No spec, hence not selected:
%select_for_binding( [ _FInfo | T ], SeaplusFunIds, Acc ) ->
select_for_binding( [ #function_info{ %name=Name,
									  %arity=Arity,
									  spec=undefined } | T ],
					SeaplusFunIds, Acc ) ->

	%trace_utils:debug_fmt( "~s/~B skipped for binding (no spec).",
	%					   [ Name, Arity ] ),

	select_for_binding( T, SeaplusFunIds, Acc );


% A spec is available here:
select_for_binding( [ FInfo=#function_info{ name=Name,
											arity=Arity } | T ],
					SeaplusFunIds, Acc ) ->

	FunId = { Name, Arity },

	case lists:member( FunId, SeaplusFunIds ) of

		true ->
			%trace_utils:debug_fmt(
			%  "Seaplus-defined ~s/~B not selected in binding.",
			%  [ Name, Arity ] ),
			select_for_binding( T, SeaplusFunIds, Acc );

		false ->
			%trace_utils:debug_fmt( "~s/~B selected in binding.",
			%					   [ Name, Arity ] ),
			select_for_binding( T, SeaplusFunIds, [ FInfo | Acc ] )

	end.




% Either generate (if no clause defined) or transform (otherwise) the listed API
% functions.
%
post_process_fun_infos( FunInfos, PortDictKey, ExportLoc, DefLoc ) ->

	% Prepare for the transformation of any user-implemented function:

	TransformTable = table:new( [ { 'call', fun call_transformer/4 } ] ),

	% A template record used to transform each already-implemented API function:
	Transform = #ast_transforms{ transform_table=TransformTable },

	post_process_fun_infos( FunInfos, PortDictKey, ExportLoc, DefLoc,
							Transform, _Acc=[], _Count=1 ).


% (helper)
post_process_fun_infos( _FunInfos=[], _PortDictKey, _ExportLoc, _DefLoc,
						_Transform, Acc, _Count ) ->
	lists:reverse( Acc );


% No clause here, hence shall be generated:
post_process_fun_infos( [ FInfo=#function_info{ %name=Name,
												arity=Arity,
												clauses=[] } | T ],
						PortDictKey, ExportLoc, DefLoc, Transform, Acc,
						Count ) ->

	FunDriverId = Count,

	%trace_utils:debug_fmt( "Assigning Driver ID #~B to ~s/~B.",
	%					   [ FunDriverId, Name, Arity ] ),

	Clauses = generate_clauses_for( FunDriverId, Arity, PortDictKey ),

	%trace_utils:debug_fmt( "Generated clauses:~n~p", [ Clauses ] ),

	NewFInfo = FInfo#function_info{

		% Otherwise 'undefined', ending up at the beginning of the AST, prior to
		% defines for example:
		%
		location=DefLoc,

		% Otherwise 'undefined', rejected by the linter:
		line=0,

		clauses=Clauses,
		exported=[ ExportLoc ] },

	post_process_fun_infos( T, PortDictKey, ExportLoc, DefLoc, Transform,
							[ NewFInfo | Acc ], Count + 1 );


% Here, clauses are available; they have to be transformed, as the user is not
% supposed to guess:
%
% - the port key (as such a key can be statically determined, it is better to
% hardcode it with its right immediate value here at compilation time, rather
% than trigger an avoidable function call at runtime); so the pseudo-call to
% seaplus:get_service_port_key/0 is to be replaced by the right, service
% specific, key
%
% - the function driver identifier; so the pseudo-call to
% seaplus:get_function_driver_id/0 is to be replaced by the right id
%
post_process_fun_infos( [ FInfo=#function_info{ name=Name,
												arity=Arity,
												clauses=Clauses } | T ],
						PortDictKey, ExportLoc, DefLoc, Transform, Acc,
						Count ) ->

	FunId = { Name, Arity },

	FunDriverId = Count,

	% Just update the right fields of the record template:
	ThisTransform = Transform#ast_transforms{
					  transformed_function_identifier=FunId,
					  transformation_state={ PortDictKey, FunDriverId } },

	% And apply that Seaplus transform:
	{ NewClauses, _NewTransform } =
		ast_clause:transform_function_clauses( Clauses, ThisTransform ),

	NewFInfo = FInfo#function_info{

				 clauses=NewClauses,

				 % The user is not expected to export the functions he defined:
				 exported=[ ExportLoc ] },

	post_process_fun_infos( T, PortDictKey, ExportLoc, DefLoc, Transform,
							[ NewFInfo | Acc ], Count + 1 ).



% Performs the AST substitutions in the user-provided clauses.
%
% (anonymous mute variables correspond to line numbers)
%
-spec call_transformer( ast_base:line(),
				ast_expression:function_ref_expression(),
				ast_expression:params_expression(), ast_transforms() ) ->
					  { [ ast_expression:ast_expression() ], ast_transforms() }.
% Replacing here seaplus:get_service_port_key() with PortDictKey value:
call_transformer( _LineCall, _FunctionRef={ remote, _, {atom,_,seaplus},
											{atom,Line,get_service_port_key} },
				  _Params=[],
				  Transforms=#ast_transforms{
					transformation_state={ PortDictKey, _FunDriverId } } ) ->

	% (no possible extra/inner recursive transformation)

	NewExpr = { atom, Line, PortDictKey },

	{ [ NewExpr ], Transforms };


% Replacing here seaplus:get_function_driver_id() with FunDriverId value:
call_transformer( _LineCall,
				  _FunctionRef={ remote, _, {atom,_,seaplus},
								 {atom,Line,get_function_driver_id} },
				  _Params=[],
				  Transforms=#ast_transforms{
					transformation_state={ _PortDictKey, FunDriverId } } ) ->

	% (no possible extra/inner recursive transformation)

	NewExpr = { atom, Line, FunDriverId },

	{ [ NewExpr ], Transforms };


% Other elements left as are (but recursed into):
call_transformer( LineCall, FunctionRef, Params, Transforms ) ->

	{ NewParams, _ParamsTransforms } =
		ast_expression:transform_expressions( Params, Transforms ),

	NewExpr = { 'call', LineCall, FunctionRef, NewParams },

	{ [ NewExpr ], Transforms }.



% Returns the identifiers of the function introduced by Seaplus in a service
% module.
%
get_seaplus_function_ids() ->

	% Only coming from seaplus.hrl, none added by the Seaplus parse transform:
	%
	% (note that no explicit filtering is done based on whether or not they are
	% for example exported)
	%
	[ {start,0}, {start_link,0}, {restart,0}, {stop,0}, {activate_seaplus,1} ].



% Returns the (atom) key under which the corresponding port will be stored:
%
% (must agree with seaplus:get_service_port_key_for/1; not called directly here
% as we prefer have this parse transform and the seaplus module not depending on
% each other)
%
-spec get_port_dict_key_for( module_info() ) -> dict_key().
get_port_dict_key_for( #module_info{ module={ ModName, _Loc } } ) ->
	KeyString = text_utils:format( "_seaplus_port_for_service_~s",
								   [ ModName ] ),
	text_utils:string_to_atom( KeyString ).



% Generates the clauses for specified function, like for:
%
% bar( A, B ) ->
%	seaplus:call_port_for( ?seaplus_foobar_port_dict_key, 5, [ A, B ] ).
%
% (helper)
%
-spec generate_clauses_for( function_driver_id(), arity(), dict_key() ) ->
								  [ meta_utils:clause_def() ].
generate_clauses_for( Id, Arity, PortDictKey ) ->

	Line = 0,

	[ { clause, Line,
		ast_generation:get_header_params( Arity ), _Guards=[],
		[ { call, Line,
			{ remote, Line, {atom,Line,seaplus}, {atom,Line,call_port_for} },
			[ {atom,Line,PortDictKey},
			  {integer,Line,Id},
			  ast_generation:enumerated_variables_to_form( Arity ) ] } ] } ].



% Reinjects specified function infos into specified module info.
-spec reinject_fun_infos( [ function_info() ], module_info() ) -> module_info().
reinject_fun_infos( FunInfos,
					ModuleInfo=#module_info{ functions=FunctionTable } ) ->
	FullFunctionTable = inject_fun_infos( FunInfos, FunctionTable ),
	ModuleInfo#module_info{ functions=FullFunctionTable }.



% (helper)
inject_fun_infos( _FunInfos=[], FunctionTable ) ->
	FunctionTable;

inject_fun_infos( [ FunInfo=#function_info{ name=Name, arity=Arity } | T ],
				  FunctionTable ) ->
	% An already-existing (clauseless) entry is expected:
	NewFunctionTable = table:updateEntry( _Id={Name,Arity}, FunInfo,
										  FunctionTable ),
	inject_fun_infos( T, NewFunctionTable ).
