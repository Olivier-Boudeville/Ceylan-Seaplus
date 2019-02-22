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

% For , etc.:
%-include("seaplus_info.hrl").




% Local shorthands:

-type ast() :: ast_base:ast().
%-type located_form() :: ast_info:located_form().
-type module_info() :: ast_info:module_info().
%-type function_info() :: ast_info:function_info().
%-type function_table() :: ast_info:function_table().


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

	% Then promote this Myriad-level information into a Seaplus one:
	% (here is the real Seaplus magic)
	%
	ProcessedModuleInfo = process_module_info_from( InputModuleInfo ),

	%trace_utils:debug_fmt(
	%  "Module information after Seaplus transformation: ~s",
	%  [ ast_info:module_info_to_string( ProcessedModuleInfo ) ] ),

	?display_trace( "Module information processed, "
					"recomposing corresponding AST." ),

	OutputAST = ast_info:recompose_ast_from_module_info(
				  ProcessedModuleInfo ),

	%trace_utils:debug_fmt( "Seaplus output AST:~n~p", [ OutputAST ] ),

	%OutputASTFilename = text_utils:format(
	%           "Seaplus-output-AST-for-module-~s.txt",
	%			[ element( 1, TransformedModuleInfo#module_info.module ) ] ),

	%ast_utils:write_ast_to_file( OutputAST, OutputASTFilename ),

	%ast_utils:write_ast_to_file( lists:sort( OutputAST ),
	%							 "Seaplus-output-AST-sorted.txt" ),

	{ OutputAST, ProcessedModuleInfo }.




% Applies the actual Seaplus transformations.
-spec process_module_info_from( module_info() ) -> module_info().
process_module_info_from( ModuleInfo ) ->
	ModuleInfo.
