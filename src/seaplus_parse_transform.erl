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
% Meant, for a Foobar service, to operate on a foobar_service.erl stub, so that:
%
% - a fully-functional foobar_service module becomes available
%
% - a corresponding foobar_service.h C header is generated in order to ease the
% development of the corresponding C-side driver
%
-module(seaplus_parse_transform).



% Implementation notes:

% Calls in turn the Myriad parse transform, before and after the Seaplus-level
% operations have been completed (respectively to obtain a module_info as input
% for Seaplus, and to transform adequately, as standard Erlang code, any
% Seaplus-injected code that would rely on Myriad conventions).
%
% One will get: 'undefined parse transform 'wooper_parse_transform'' as soon as
% a compiled module called by the parse transform (ex: text_utils.beam) will not
% be found (hence even if the transform itself is available) or a non-exported
% (or even not existing) function is called (ex: text_utils:format/1).

% We must discriminate here between methods and functions, and identify, among
% detected methods: the requests, the oneways and the static ones.
%
% For that we can rely either on the type specs (if any, as technically they
% remain optional - but we decided that, conventionally, they should better be
% mandatory) or on the function definition itself (relying then on the Seaplus
% return primitives).
%
% More precisely, both for the type spec and the actual code (all clauses):
%
% - a request shall return its state and value thanks to a call to
% wooper:request_return/2
%
% - a oneway shall return its state thanks to a call to wooper:oneway_return/1
%
% - a static method (as opposed to the previous two member methods) shall return
% this value thanks to a call to wooper:static_return/1


% We consider here that the ?table type (defined in meta_utils.hrl) is actually
% map_hashtable, and thus can be designated as just table.


% Regarding the Seaplus parse transform.

% All Seaplus-related symbols (ex: atoms, functions, etc.) are to be prefixed by
% 'wooper_'. This prefix shall be considered as reserved for Seaplus internals
% (all wooper_* symbols are forbidden to the user).

% Previously, for simplicity, some values (ex: the superclasses) were defined
% thanks to macro defines (ex: '-define( wooper_foo, 42 )'). Now they are
% specified thanks to attributes -ex: '-wooper_foo( 42 ).' and when there was
% previously ?wooper_foo, we replaced that with the definition of a
% wooper_get_foo() function.



% Regarding function/method exports:
%
% We could have preferred that methods are auto-exported (defining them would
% have sufficed, with no particular export declaration), yet:
%
% - a pseudo-export line (i.e. '-export([ setColor/2, ...]).') would have to be
% generated and placed
%
% - knowing that we want -spec lines (even in the form '-oneway setColor(...,
% ... ) -> ...') to remain optional, the kind of a method
% (oneway/request/static) would have to be inferred at compilation-time, which
% is not easy (ex: scanning for wooper:oneway_return/1 through all "leaves" of
% the call graph); moreover, all standard functions could be static ones, and
% thus would be exported, which is not desirable
%
% As a result, we preferred relying on explicit exports:
%
% - '-export([ f/1, g/0 ])' for simple functions, as usual
% - '-oneway_export([ setColor/2, resetFoo/1 ]) for oneway methods
% - '-request_export([ getColor/1, isFooReset/1 ]) for request methods
% - '-static_export([ get_mean_count/1, get_default_name/0 ]) for static methods
%
% As for constructor(s) and destructor (if any), there are auto-exported (i.e.
% all construct/N and destruct/1 functions).


% Regarding function/method type specifications:
%
% - example for a (plain) function:
%       -spec f( float() ) -> integer().
%
% - example for a oneway method:
%       -oneway_spec setColor( wooper:state(), color() ) -> void().
%
% - example for a request method:
%       -request_spec getColor( wooper:state() ) -> color().
%
% - example for a static method:
%       -static_spec get_mean_count( foo() ) -> count().


% Used for iterated (re)composition of class information:
-type compose_pair() :: { ast_info:function_table(), class_info() }.

% For clarity:
-type operator_table() :: function_table().

-export_type([ compose_pair/0, operator_table/0 ]).


-export([ run_standalone/1, run_standalone/2,
		  parse_transform/2, apply_wooper_transform/1,
		  generate_class_info_from/1, generate_module_info_from/1 ]).


% The specific type of an Erlang function, from the Seaplus point of view:
%
-type function_type() :: 'constructor' | 'destructor'
					   | 'request' | 'oneway' | 'static' | 'function'.



% For function_info:
-include("ast_info.hrl").

% For class_info, attribute_info, etc.:
-include("wooper_info.hrl").




% Local shorthands:

-type ast() :: ast_base:ast().
-type located_form() :: ast_info:located_form().
-type module_info() :: ast_info:module_info().
-type function_info() :: ast_info:function_info().
-type function_table() :: ast_info:function_table().

-type class_info() :: wooper_info:class_info().


% For debugging:
-export([ check_class_info/1 ]).


% tmp:
-export([ add_function/4, add_request/4, add_oneway/4, add_static/4,
		  identify_function/3, infer_function_type/1, infer_fun_type/2,
		  get_new_variation_names/0, get_attribute_forms/1 ]).



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
-spec run_standalone( file_utils:file_name() ) -> { ast(), class_info() }.
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
			  [ ast_utils:preprocessor_option() ] ) -> { ast(), class_info() }.
run_standalone( FileToTransform, PreprocessorOptions ) ->

	InputAST = ast_utils:erl_to_ast( FileToTransform, PreprocessorOptions ),

	% Returns { SeaplusAST, ClassInfo }:
	apply_wooper_transform( InputAST ).



% The parse transform itself, transforming the specified (Seaplus-based) Abstract
% Format code first into a Myriad-based information being itself converted in
% turn into an Erlang-compliant Abstract Format code.
%
-spec parse_transform( ast(), list() ) -> ast().
parse_transform( InputAST, _Options ) ->

	%trace_utils:trace_fmt( "Seaplus input AST:~n~p~n", [ InputAST ] ),

	%trace_utils:trace_fmt( "Seaplus options:~n~p~n", [ Options ] ),

	%ast_utils:write_ast_to_file( InputAST, "Seaplus-input-AST.txt" ),

	% In the context of this direct parse transform, the class_info is of no
	% use afterwards and thus can be dropped:
	%
	{ SeaplusAST, _ClassInfo } = apply_wooper_transform( InputAST ),

	%trace_utils:trace_fmt( "Seaplus output AST:~n~p~n", [ SeaplusAST ] ),

	%ast_utils:write_ast_to_file( SeaplusAST, "Seaplus-output-AST.txt" ),

	SeaplusAST.



% Transforms specified AST for Seaplus.
%
-spec apply_wooper_transform( ast() ) -> { ast(), class_info() }.
apply_wooper_transform( InputAST ) ->

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
	ClassInfo = generate_class_info_from( InputModuleInfo ),

	?display_trace( "Class information generated, "
					   "transforming it." ),

	% Finally perform Seaplus-specific transformation:
	NewClassInfo = transform_class_info( ClassInfo ),

	%trace_utils:debug_fmt( "Transformed class information: ~s",
	%			   [ wooper_info:class_info_to_string( NewClassInfo ) ] ),

	?display_trace( "Generating back module information." ),

	% Then translates back this class information in module information:
	NewModuleInfo = generate_module_info_from( NewClassInfo ),

	% Allows to have still functional class modules during the Seaplus
	% developments, by bypassing the Seaplus transformations as a whole:
	%
	EnableSeaplusParseTransform = true,
	%EnableSeaplusParseTransform = false,

	ModuleInfoOfInterest = case EnableSeaplusParseTransform of

		true ->
			NewModuleInfo;

		false ->
			% Do-nothing operation then:
			InputModuleInfo

	end,

	%trace_utils:debug_fmt(
	%  "Module information just prior to Myriad transformation: ~s",
	%  [ ast_info:module_info_to_string( ModuleInfoOfInterest ) ] ),

	% And finally obtain the corresponding updated AST thanks to Myriad:
	%
	% (should be done as a final step as Seaplus may of course rely on
	% Myriad-introducted facilities such as void, maybe, table, etc.)

	?display_trace( "Performing Myriad-level transformation." ),

	{ TransformedModuleInfo, _MyriadTransforms } =
		myriad_parse_transform:transform_module_info(
							  ModuleInfoOfInterest ),

	%trace_utils:debug_fmt(
	%  "Module information after Myriad transformation: ~s",
	%  [ ast_info:module_info_to_string( TransformedModuleInfo ) ] ),

	OutputAST = ast_info:recompose_ast_from_module_info(
				  TransformedModuleInfo ),

	?display_trace( "Recomposing corresponding AST." ),

	%trace_utils:debug_fmt( "Seaplus output AST:~n~p", [ OutputAST ] ),

	%OutputASTFilename = text_utils:format(
	%           "Seaplus-output-AST-for-module-~s.txt",
	%			[ element( 1, TransformedModuleInfo#module_info.module ) ] ),

	%ast_utils:write_ast_to_file( OutputAST, OutputASTFilename ),

	%ast_utils:write_ast_to_file( lists:sort( OutputAST ),
	%							 "Seaplus-output-AST-sorted.txt" ),

	{ OutputAST, NewClassInfo }.




% Returns the class-level information that were gathered from the specified
% module-level ones.
%
% (reciprocal of generate_module_info_from/1)
%
-spec generate_class_info_from( module_info() ) -> class_info().
generate_class_info_from( ModuleInfo ) ->

	% We handle there only Seaplus-specific needs:

	ExtractedClassInfo = create_class_info_from( ModuleInfo ),

	% Optional:
	check_class_info( ExtractedClassInfo ),

	ExtractedClassInfo.



% Recomposes (Seaplus) class information from (Myriad) module-level ones.
%
% The goal is to pick the relevant Seaplus-level information (from the module
% info), to transform them and to populate the specified class information with
% the result.
%
-spec create_class_info_from( module_info() ) -> class_info().
create_class_info_from(
  % We basically reuse (as they are, or after relevant transformations) all
  % information gathered from the module:
  _ModuleInfo=#module_info{ module=ModuleEntry,
							compilation_options=CompileOptTable,
							compilation_option_defs=CompileOptDefs,
							parse_attributes=ParseAttrTable,
							remote_spec_defs=RemoteSpecDefs,
							includes=Includes,
							include_defs=IncludeDefs,
							type_exports=TypeExportTable,
							types=TypeTable,
							records=RecordTable,
							function_imports=FunctionImportTable,
							function_imports_defs=FunctionImportDefs,
							function_exports=FunctionExportTable,
							functions=FunctionTable,
							optional_callbacks_defs=OptCallbacksDefs,
							last_line=LastLine,
							markers=MarkerTable,
							errors=Errors,
							unhandled_forms=UnhandledForms } ) ->

	BlankClassInfo = wooper_info:init_class_info(),

	% For a starting basis, let's init first all the fields that we do not plan
	% to update, as they are:
	%
	% (the fields that will be updated afterwards are commented out, to be able
	% to check for completeness more easily)
	%
	VerbatimClassInfo = BlankClassInfo#class_info{
						  %class
						  %superclasses
						  %attributes
						  %inherited_attributes
						  compilation_options=CompileOptTable,
						  compilation_option_defs=CompileOptDefs,
						  parse_attributes=ParseAttrTable,
						  remote_spec_defs=RemoteSpecDefs,
						  includes=Includes,
						  include_defs=IncludeDefs,
						  type_exports=TypeExportTable,
						  types=TypeTable,
						  records=RecordTable,
						  function_imports=FunctionImportTable,
						  function_imports_defs=FunctionImportDefs,
						  function_exports=FunctionExportTable,
						  functions=FunctionTable,
						  %constructors
						  %destructor
						  %request_exports
						  %requests
						  %oneway_exports
						  %oneways
						  %static_exports
						  %statics
						  optional_callbacks_defs=OptCallbacksDefs,
						  last_line=LastLine,
						  markers=MarkerTable,
						  errors=Errors,
						  unhandled_forms=UnhandledForms },


	% Then taking care of the missing fields, roughly in their original order:

	ClassInClassInfo = wooper_class_management:manage_classname( ModuleEntry,
															VerbatimClassInfo ),

	SuperClassInfo = wooper_class_management:manage_superclasses(
					   ParseAttrTable, ClassInClassInfo ),


	AttrClassInfo = wooper_state_management:manage_attributes( SuperClassInfo ),

	% We extract elements (ex: constructors) from the function table, yet we do
	% not modify specifically the other related information (ex: exports).

	% We manage here { FunctionTable, ClassInfo } pairs, in which the first
	% element is the reference, most up-to-date version of the function table
	% that shall be used (extracted-out for convenience) - not any counterpart
	% that could be found in the second element and that will be updated later
	% from the first:
	%
	InitialFunctionTable = AttrClassInfo#class_info.functions,

	InitialPair = { InitialFunctionTable, AttrClassInfo },


	ConstructPair = wooper_instance_construction:manage_constructors(
					  InitialPair ),

	DestructPair = wooper_instance_destruction:manage_destructor(
					 ConstructPair ),

	MethodPair = wooper_method_management:manage_methods( DestructPair ),


	% ...

	_FinalPair = { FinalFunctionTable, FinalClassInfo } = MethodPair,

	ReturnedClassInfo = FinalClassInfo#class_info{
						  functions=FinalFunctionTable },

	%trace_utils:debug_fmt( "Recomposed class information: ~s",
	%	   [ wooper_info:class_info_to_string( ReturnedClassInfo ) ] ),

	ReturnedClassInfo.



% Class/module section:

% Any invalid or duplicated module declaration will be caught by the compiler
% anyway.


% We wanted the users to rely on a define such as '-classname(class_MyName)'
% instead of '-module(class_MyName)', yet apparently -module should be found
% *before* the parse-transform is ever triggered (we collected the very first
% InputAST we can get to check, it is already unusable if a module declaration
% was lacking), so that the preprocessor can rely on the ?MODULE macro
% afterwards; otherwise the input AST contains forms such as
% '{error,{L,epp,{undefined,'MODULE',none}}}' instead of the forms that referred
% to ?MODULE (as a result these are lost, unrecoverable information).
%
% Only possible work-around: have the actual modules compiled by a specific
% program, driving the compilation by itself, instead of being inserted as a
% mere parse transform. Later maybe!
%
% For the moment, we stick to requiring a
% -module(class_XXX) declaration.
%
%% get_info( _AST=[ { 'attribute', Line, 'classname', Classname } | T ],
%%		  C=#class_info{ class=undefined, class_def=undefined } ) ->

%%	trace_utils:debug_fmt( "Intercepting Seaplus classname declaration for "
%%						   "'~s'.", [ Classname ] ),

%%	check_classname( Classname ),

%%	% Transforms that in a standard module definition:
%%	NewDef = { 'attribute', Line, 'module', Classname },

%%	get_info( T, C#class_info{ class=Classname, class_def=NewDef } );


%% % We accept (only) the Erlang-standard, direct '-module(XXX).' declaration
%% for % now:

%% get_info( _AST=[ F={ 'attribute', _Line, 'module', Classname } | T ],
%%		  C=#class_info{ class=undefined, class_def=undefined } ) ->

%%	%trace_utils:debug_fmt( "Intercepting module-based classname declaration "
%%	%					   "for '~s'.", [ Classname ] ),

%%	check_classname( Classname ),

%%	get_info( T, C#class_info{ class=Classname, class_def=F } );


%% % The fact that no '-module(XXX).' can be found in the source file results in
%% % forms such as {error,{85,epp,{undefined,'MODULE',none}}} that we want to
%% % filter-out, as we will introduce a relevant module form afterwards:
%% %
%% get_info( _AST=[ F={ 'error',{ _Line, 'epp',
%%								 { 'undefined', 'MODULE', 'none' } } } | T ],
%%		  C ) ->

%%	% Problems ahead:
%%	trace_utils:debug_fmt( "Dropping module-related error form ~p.", [ F ] ),

%%	get_info( T, C );



% Adds specified function into the corresponding table.
%
add_function( Name, Arity, Form, FunctionTable ) ->

	FunId = { Name, Arity },

	% Its spec might have been found before its definition:

	FunInfo = case table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->
					  % New entry then:
					  #function_info{ name=Name,
									  arity=Arity,
									  location=undefined,
									  line=undefined,
									  clauses=Form
									  % Implicit:
									  %spec=undefined
									  %callback=undefined
									  %exported=[]
									 };

		{ value, F=#function_info{ clauses=undefined } } ->
			% Just add the form then:
			F#function_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			wooper_internals:raise_error( { multiple_definition_for, FunId } )

	end,

	table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ).



% Adds specified request into the corresponding table.
%
add_request( Name, Arity, Form, RequestTable ) ->

	RequestId = { Name, Arity },

	% Its spec might have been found before its definition:

	RequestInfo = case table:lookupEntry( RequestId, RequestTable ) of

		key_not_found ->
			% New entry then:
			#function_info{ name=Name,
							arity=Arity,
							clauses=Form
							% Implicit:
							%spec=undefined
						   };

		{ value, F=#function_info{ clauses=undefined } } ->
			% Just add the form then:
			F#function_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			wooper_internals:raise_error(
			  { multiple_definitions_for_request, RequestId } )

	end,

	table:addEntry( _K=RequestId, _V=RequestInfo, RequestTable ).



% Adds specified oneway into the corresponding table.
%
add_oneway( Name, Arity, Form, OnewayTable ) ->

	OnewayId = { Name, Arity },

	% Its spec might have been found before its definition:

	OnewayInfo = case table:lookupEntry( OnewayId, OnewayTable ) of

		key_not_found ->
			% New entry then:
			#function_info{ name=Name,
							arity=Arity,
							clauses=Form
							% Implicit:
							%spec=undefined
						  };

		{ value, F=#function_info{ clauses=undefined } } ->
			% Just add the form then:
			F#function_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			wooper_internals:raise_error(
			  { multiple_definitions_for_oneway, OnewayId } )

	end,

	table:addEntry( _K=OnewayId, _V=OnewayInfo, OnewayTable ).



% Adds specified static method into the corresponding table.
%
add_static( Name, Arity, Form, StaticTable ) ->

	StaticId = { Name, Arity },

	% Its spec might have been found before its definition:

	StaticInfo = case table:lookupEntry( StaticId, StaticTable ) of

		key_not_found ->
			% New entry then:
			#function_info{ name=Name,
							arity=Arity,
							clauses=Form
							% Implicit:
							%spec=undefined
						  };

		{ value, F=#function_info{ clauses=undefined } } ->
			% Just add the form then:
			F#function_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			wooper_internals:raise_error(
			  { multiple_definitions_for_static, StaticId } )

	end,

	table:addEntry( _K=StaticId, _V=StaticInfo, StaticTable ).



% Returns the (located) forms that correspond to known (class-level) attributes.
%
-spec get_attribute_forms( wooper_info:attribute_table() ) ->
								 [ located_form() ].
get_attribute_forms( _AttributeTable ) ->

	% Currently not managed (no consequence of class-level attribute declaration
	% onto the resulting AST:
	[].

	% The key (attribute name) is duplicated in the attribute_info value:
	% ... = table:values( AttributeTable ),




% Tells whether specified clause belongs to a request, a oneway, a constuctor,
% etc.
%
-spec identify_function( basic_utils:function_name(), arity(),
						 basic_utils:fixme() ) -> function_type().
identify_function( _Name=construct, _Arity, _Clause ) ->
	constructor;

identify_function( _Name=destruct, _Arity=1, _Clause ) ->
	destructor;

identify_function( _Name=destruct, Arity, _Clause ) ->
	wooper_internals:raise_error( { destructor_arity_must_be_one, Arity } );

identify_function( Name, _Arity,
				   _Clause={ clause, _Line, _Vars, _, AST } ) ->

	case lists:member( Name, get_new_variation_names() ) of

		true ->
			%wooper_internals:raise_error(
			%    { new_variations_are_reserved, Name } );
			fixme;

		false ->
			ok

	end,

	StringName = text_utils:atom_to_string( Name ),

	case StringName of

		"wooper" ++ _ ->
			%wooper_internals:raise_error(
			%    { wooper_prefix_is_reserved, Name } );
			fixme;

		_ ->
			ok

	end,

	%trace_utils:debug_fmt( "Inferring ~p/~B.", [ Name, Arity ] ),

	% We have here either a function, a request, a oneway or a static method. To
	% discriminate, we simply rely on how values are returned:
	infer_function_type( AST ).



% Infers the type of a function based on the code of one of its clauses.
%
% Note that a method *must* return:
%
% 1. with an appropriate Seaplus construct (ex: wooper:oneway_result/1)
%
% 2. directly from its body (not from an helper function being called)
%
infer_function_type( AST ) ->

	%trace_utils:debug_fmt( "Inferring from code: ~p", [ AST ] ),

	% If no wooper return pseudo-call is detected, by default it will be a
	% function:
	%
	{ _SameAST, FunType } = ast_transform:traverse_term( _TargetTerm=AST,
									 _TypeDescription=tuple,
									 _TermTransformer=fun infer_fun_type/2,
									 _UserData=function ),

	FunType.



% A meta_utils:term_transformer():
%
% We simply look-up elements like:
%
% { call, L1,
%         { remote, L2,
%                  { atom, L3, wooper },
%                  { atom, L4, oneway_return }
%         },
%         [ { var, L5, 'AState' } ]
% }
% and determine the type from, here, oneway_return.
%
-spec infer_fun_type( term(), basic_utils:user_data() ) ->
								   { term(), basic_utils:user_data() }.
infer_fun_type( Term={ call, CallLine, { remote, _L2,
										 { atom, _L3, wooper },
										 { atom, _L4, Return } },
					   ArgList }, CurrentType )->

	Len = length( ArgList ),

	DetectedType = case Return of

		request_return when Len =:= 2 ->
			request;

		oneway_return when Len =:= 1 ->
			oneway;

		static_return ->
			static;

		_OtherFunction ->
			CurrentType

	end,

	% As we do not have a way to stop the transformation, we take advantage of
	% that to check consistency:
	%
	NewType = case CurrentType of

		% Possibly overriding defaults:
		function ->
			DetectedType;

		% Matches, confirms detection:
		DetectedType ->
			DetectedType;

		OtherType ->
			wooper_internals:raise_error( { inconsistent_function_type,
											CurrentType, OtherType, CallLine } )

	end,

	{ Term, NewType };


infer_fun_type( Term, CurrentType ) ->
	{ Term, CurrentType }.




% Ensures that the described class respects appropriate constraints for Seaplus
% generation, besides the ones checked during the AST exploration and the ones
% that will be checked by the compiler.
%
-spec check_class_info( class_info() ) -> void().
check_class_info( #class_info{ constructors=Constructors } ) ->

	case table:isEmpty( Constructors ) of

		true ->
			wooper_internals:raise_error( no_constructor_defined );

		false ->
			ok

	end.

	% For each clause of each constructor, we should check that the constructors
	% of direct superclasses have all a fair chance of being called.



% Returns a list of the names of the class_X:*new* operators that are generated
% by Seaplus to branch on the construct/N and thus shall not be defined by the
% user.
%
get_new_variation_names() ->
	[ new_link, synchronous_new, synchronous_new_link, synchronous_timed_new,
	  synchronous_timed_new_link, remote_new, remote_new_link,
	  remote_synchronous_new, remote_synchronous_new_link,
	  remote_synchronisable_new_link, remote_synchronous_timed_new,
	  remote_synchronous_timed_new_link ].



% Transforms (at the Seaplus level) specified class information.
%
-spec transform_class_info( class_info() ) -> class_info().
transform_class_info( ClassInfo ) ->
	% Nothing specific done currently!
	ClassInfo.



% Generates back (Myriad-level) module-level information from specified
% class-level information.
%
% (reciprocal of generate_class_info_from/1)
%
-spec generate_module_info_from( class_info() ) -> module_info().
generate_module_info_from( #class_info{
				 class=ClassEntry,
				 superclasses={ _Superclasses, MaybeSuperclassesLocDef },

				 attributes=AttributeTable,

				 % No impact onto the class-related module itself:
				 inherited_attributes=_InheritedAttributeTable,

				 compilation_options=CompileOptTable,
				 compilation_option_defs=CompileOptDefs,

				 parse_attributes=ParseAttrTable,

				 remote_spec_defs=RemoteSpecDefs,

				 includes=Includes,
				 include_defs=IncludeDefs,

				 type_exports=TypeExportTable,
				 types=TypeTable,

				 records=RecordTable,

				 function_imports=FunctionImportTable,
				 function_imports_defs=FunctionImportDefs,

				 function_exports=FunctionExportTable,
				 functions=FunctionTable,

				 constructors=ConstructorTable,
				 new_operators=OperatorTable,
				 destructor=MaybeDestructor,

				 request_exports=_RequestExportTable,
				 requests=RequestTable,

				 oneway_exports=_OnewayExportTable,
				 oneways=OnewayTable,

				 static_exports=_StaticExportTable,
				 statics=StaticTable,

				 optional_callbacks_defs=OptCallbackDefs,

				 last_line=LastLine,

				 markers=MarkerTable,

				 errors=Errors,

				 unhandled_forms=UnhandledForms } ) ->

	% Adds back the relevant (Seaplus-related) parse attributes that were
	% interpreted (even though they might not be so useful now):
	%
	FullParseAttrTable = gather_parse_attributes( ParseAttrTable,
									  MaybeSuperclassesLocDef, AttributeTable ),

	% In addition to the plain, classical functions already in
	% FunctionExportTable and Functions, we have to add back constructors,
	% destructor and methods in the function-related fields, i.e. regarding
	% export and definition:

	% For constructors:
	WithConstrFunTable = lists:foldl(

		fun( { ConstructArity, ConstructFunInfo }, AccFunTable ) ->
			ConstructId = { construct, ConstructArity },
			% Expected to have already been appropriately exported.
			table:addNewEntry( ConstructId, ConstructFunInfo,
							   AccFunTable )
		end,
		_Acc0=FunctionTable,
		_List=table:enumerate( ConstructorTable ) ),

	% For new operators:

	% (we do not use merge/2, as we want to detect any clash between
	% user-defined functions and these automatically-generated new operators):
	%

	%trace_utils:debug_fmt( "Integrating the new operators: ~p",
	%					   [ table:keys( OperatorTable ) ] ),

	WithNewOpFunTable = register_functions( table:enumerate( OperatorTable ),
											WithConstrFunTable ),

	% For destructor:
	WithDestrFunTable = case MaybeDestructor of

		undefined ->
			WithNewOpFunTable;

		DestructFunInfo ->
			DestructId = { destruct, 1 },
			% Expected to have already been appropriately exported.
			table:addNewEntry( DestructId, DestructFunInfo,
							   WithNewOpFunTable )

	end,

	% For methods:

	% Probably useless now that locations are determined directly if implicit:
	WithMthdExpTable = FunctionExportTable,
	AllExportTable = WithMthdExpTable,

	WithMthdFunTable = wooper_method_management:methods_to_functions(
				RequestTable, OnewayTable, StaticTable, WithDestrFunTable,
				MarkerTable ),

	AllFunctionTable = WithMthdFunTable,

	%trace_utils:debug_fmt( "Complete function table: ~s",
	%					   [ table:toString( AllFunctionTable ) ] ),

	% Directly returned (many fields can be copied verbatim):
	#module_info{

		% Untouched:
		module=ClassEntry,
		compilation_options=CompileOptTable,
		compilation_option_defs=CompileOptDefs,

		parse_attributes=FullParseAttrTable,

		% Untouched:
		remote_spec_defs=RemoteSpecDefs,
		includes=Includes,
		include_defs=IncludeDefs,
		type_exports=TypeExportTable,
		types=TypeTable,
		records=RecordTable,
		function_imports=FunctionImportTable,
		function_imports_defs=FunctionImportDefs,
		function_exports=AllExportTable,
		functions=AllFunctionTable,
		optional_callbacks_defs=OptCallbackDefs,
		last_line=LastLine,
		markers=MarkerTable,
		errors=Errors,
		unhandled_forms=UnhandledForms }.



% Recreates a complete table of parse attributes, from specified arguments.
%
-spec gather_parse_attributes( ast_info:attribute_table(),
	   maybe( ast_info:located_form() ), wooper_info:attribute_table()  ) ->
									 ast_info:attribute_table().
gather_parse_attributes( ParseAttrTable, MaybeSuperclassesLocDef,
						 _AttributeTable ) ->

	% AttributeTable not yet populated.

	case MaybeSuperclassesLocDef of

		undefined ->
			ParseAttrTable;

		SuperclassesLocDef ->
			% V must be a list of {AttrValue,LocForm} pairs:
			table:addNewEntry( _K=superclasses,
							   _V=[ SuperclassesLocDef ], ParseAttrTable )

	end.


% Registers specified functions in specified (function) table, detecting
% properly any clash.
%
-spec register_functions( [ { meta_utils:function_id(), function_info() } ],
							function_table() ) -> function_table().
register_functions( [], FunctionTable ) ->
	FunctionTable;

register_functions( [ { FunId, FunInfo } | T ], FunctionTable ) ->
	case table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->
			NewFunctionTable = table:addEntry( FunId, FunInfo, FunctionTable ),
			register_functions( T, NewFunctionTable );

		{ value, OtherFunInfo } ->
			{ FunName, FunArity } = FunId,
			ast_utils:display_error( "Attempt to declare ~s/~B more than once; "
									 "whereas already registered as:~n  ~s~n"
									 "this function has been declared again, "
									 "as:~n  ~s~n",
				[ FunName, FunArity,
				  ast_info:function_info_to_string( OtherFunInfo ),
				  ast_info:function_info_to_string( FunInfo ) ] ),
			throw( { multiple_declarations_for, FunId } )

	end.
