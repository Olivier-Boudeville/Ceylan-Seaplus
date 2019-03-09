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


-export_type([  ]).


-export([ run_standalone/1, run_standalone/2,
		  parse_transform/2, apply_seaplus_transform/1 ]).



% For function_info:
-include("ast_info.hrl").

-type dict_key() :: atom().


% Local shorthands:

-type ast() :: ast_base:ast().
-type location() :: ast_base:form_location().
-type module_info() :: ast_info:module_info().
-type function_info() :: ast_info:function_info().

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



% For debugging:
-export([]).




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

	% Returns { SeaplusAST, ModuleInfo }:
	apply_seaplus_transform( InputAST ).



% The parse transform itself, generating notably (Myriad-based) Abstract Format
% code, before being itself converted in turn into an Erlang-compliant Abstract
% Format code.
%
-spec parse_transform( ast(), list() ) -> ast().
parse_transform( InputAST, _Options ) ->

	%trace_utils:trace_fmt( "Seaplus input AST:~n~p~n", [ InputAST ] ),

	%trace_utils:trace_fmt( "Seaplus options:~n~p~n", [ Options ] ),

	%ast_utils:write_ast_to_file( InputAST, "Seaplus-input-AST.txt" ),

	% In the context of this direct parse transform, the module_info is of no
	% use afterwards and thus can be dropped:
	%
	{ SeaplusAST, _ModuleInfo } = apply_seaplus_transform( InputAST ),

	%trace_utils:trace_fmt( "Seaplus output AST:~n~p~n", [ SeaplusAST ] ),

	%ast_utils:write_ast_to_file( SeaplusAST, "Seaplus-output-AST.txt" ),

	SeaplusAST.



% Transforms specified AST for Seaplus.
-spec apply_seaplus_transform( ast() ) -> { ast(), module_info() }.
apply_seaplus_transform( InputAST ) ->

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
			process_module_info_from( ShrunkModuleInfo )

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
-spec process_module_info_from( module_info() ) -> module_info().
process_module_info_from(
  ModuleInfo=#module_info{ module={ ModName, _Loc } } ) ->

	SelectFunInfos = identify_api_functions( ModuleInfo ),

	SelectFunIds = [ { Name, Arity }
			 || #function_info{ name=Name, arity=Arity } <- SelectFunInfos ],

	FullModuleInfo = case SelectFunIds of

		[] ->
			trace_utils:debug( "No API function detected." );

		_ ->

			trace_utils:debug_fmt( "Selected ~B functions as API elements: ~s",
				[ length( SelectFunIds ), text_utils:strings_to_string(
					[ ast_info:function_id_to_string( Id )
					  || Id <- SelectFunIds ] ) ] ),

			% Generating the header for the driver:
			generate_driver_header( ModName, SelectFunIds ),

			% Key in the process dictionary under which the service port will be
			% stored:
			%
			ServicePortDictKey = get_port_dict_key_for( ModuleInfo ),

			trace_utils:debug_fmt( "Will store the service port under the "
			   "'~s' key in the process dictionary.", [ ServicePortDictKey ] ),

			MarkerTable = ModuleInfo#module_info.markers,

			ExportLoc =
				   ast_info:get_default_export_function_location( MarkerTable ),

			DefLoc = table:getEntry( definition_functions_marker, MarkerTable ),

			WithClausesFunInfos = implement_api_functions( SelectFunInfos,
									   ServicePortDictKey, ExportLoc, DefLoc ),

			reinject_fun_infos( WithClausesFunInfos, ModuleInfo )

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
					  "/* For each of the exposed functions of the API, ~n"
					  " * a Seaplus identifier is generated to ensure that ~n"
					  " * the C code of the driver can stay in sync with the ~n"
					  " * Erlang view on said API, regardless of its changes.~n"
					  " */~n~n", [] ),

	write_mapping( HeaderFile, FunIds, _Count=1 ),

	file_utils:write( HeaderFile, "~n#endif // ~s~n", [ IncGuard ] ),

	file_utils:close( HeaderFile ).


% (helper)
write_mapping( _HeaderFile, _FunIds=[], _Count ) ->
	ok;

write_mapping( HeaderFile, _FunIds=[ { FunName, Arity } | T ], Count ) ->

	FunString = text_utils:to_uppercase( text_utils:atom_to_string( FunName ) ),

	FunSymbol = text_utils:format( "~s_~B_ID", [ FunString, Arity ] ),

	file_utils:write( HeaderFile, "#define ~s ~B~n", [ FunSymbol, Count ] ),

	write_mapping( HeaderFile, T, Count+1 ).


% Identifies the API functions.
-spec identify_api_functions( module_info() ) -> [ function_info() ].
identify_api_functions( #module_info{ functions=FunctionTable } ) ->

	% By convention, these functions are exactly the ones with a spec yet no
	% definition:
	%
	% (as a result, allows an automatic override of the default implementation)
	AllFunInfos = table:values( FunctionTable ),

	% Failed to write a proper list comprehension for that, like:
	% [ FI || FI <- AllFunInfos, FI=#function_info{ clauses=[], spec=S }
	%    andalso/when ( S =/= undefined ) ]
	%
	TargetFunInfos = get_clauseless_specs( AllFunInfos, _Acc=[] ),

	% We order the returned function_info based on their location (so that their
	% IDs correspond to their in-source order):
	%
	% (we rely on the fact that a function_info is a record whose 6th (i.e. 7-1
	% for the record tag) field is the located spec, which is a pair whose order
	% is determined first by its first element - which is the spec location)
	%
	lists:keysort( _Index=7, TargetFunInfos ).


% (helper)
get_clauseless_specs( _AllFunInfos=[], Acc ) ->
	Acc;

get_clauseless_specs( [ FInfo=#function_info{ clauses=[], spec=S } | T ], Acc )
  when S =/= undefined ->
	get_clauseless_specs( T, [ FInfo | Acc ] );

get_clauseless_specs( [ _FInfo | T ], Acc ) ->
%get_clauseless_specs( [ #function_info{ clauses=C } | T ], Acc ) ->
	%trace_utils:debug_fmt( "Read clauses: ~p", [ C ] ),
	get_clauseless_specs( T, Acc ).


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


% Generates the relevant implementation for specified functions (supposed API
% ones with a spec and no clause).
%
-spec implement_api_functions( [ function_info() ], dict_key(), location(),
							   location() ) -> [ function_info() ].
implement_api_functions( ClauselessFunctions, PortDictKey, ExportLoc, DefLoc ) ->

	implement_api_functions( ClauselessFunctions, PortDictKey, ExportLoc,
							 DefLoc, _Count=1, _Acc=[] ).


% Implements (generates clauses for) specified spec.
implement_api_functions( _ClauselessFunctions=[], _PortDictKey, _ExportLoc,
						 _DefLoc, _Count, Acc ) ->
	Acc;

implement_api_functions( [ FunctionInfo=#function_info{ %name=Name,
														arity=Arity,
														clauses=[] } | T ],
						 PortDictKey, ExportLoc, DefLoc, Count, Acc ) ->

	Id = Count,

	%trace_utils:debug_fmt( "Assigning ID #~B to ~s/~B.",
	%					   [ Id, Name, Arity ] ),

	Clauses = generate_clauses_for( Id, Arity, PortDictKey ),

	%trace_utils:debug_fmt( "Generated clauses: ~p", [ Clauses ] ),

	NewFunctionInfo = FunctionInfo#function_info{
						% Otherwise 'undefined', ending up at the beginning of
						% the AST, prior to defines for example:
						location=DefLoc,
						% Otherwise 'undefined', rejected by the linter:
						line=0,
						clauses=Clauses,
						exported=[ ExportLoc ] },

	implement_api_functions( T, PortDictKey, ExportLoc, DefLoc, Count+1,
							 [ NewFunctionInfo | Acc ] ).



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
			[{atom,Line,PortDictKey},
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
