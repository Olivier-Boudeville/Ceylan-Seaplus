/*
 * Copyright (C) 2018-2019 Olivier Boudeville
 *
 * This file is part of the Ceylan-Seaplus library.
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option)
 * any later version.
 * You can also redistribute it and/or modify it under the terms of the
 * Mozilla Public License, version 1.1 or later.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License, of the GNU General Public License and of the Mozilla Public License
 * along with this library.
 * If not, see <http://www.gnu.org/licenses/> and
 * <http://www.mozilla.org/MPL/>.
 *
 * Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
 * Creation date: Sunday, December 16, 2018
 *
 */



/*
 * C example Seaplus driver around the toy foobar library, giving access to its
 * API, i.e. to the functions declared in foobar.h (and defined on foobar.c).
 *
 * The messages from and to Erlang arrive as { FunId, FunParams } pairs, where
 * FunId is an unsigned integer and FunParams is a (possibly empty) list of
 * terms.
 *
 * These messages use a mapping to identify the (Erlang) functions whose type
 * specification was devised by the service integrator. This function identifier
 * mapping is made explicit by the foobar_seaplus_api_mapping.h header file,
 * generated by Seaplus.
 *
 * Directly inspired from:
 * http://erlang.org/doc/tutorial/erl_interface.html#c-program
 *
 * Of course such a driver can maintain any additional state (context,
 * environment, etc.) that can be made available to the actual functions exposed
 * by the service.
 *
 */


// Expected to be generated by the Seaplus parse transform, based on foobar.erl:
#include "foobar_seaplus_api_mapping.h"


// Generic helpers to facilitate the writing of this C part of the bridge:
#include "seaplus.h"


// To access to the actual C implementation of the service:
#include "foobar.h"


// For exit:
#include <stdlib.h>

//  For strcmp:
#include <string.h>


// Forward references to the introduced foobar-specific helpers:

enum foo_status get_foo_status_from_atom( const char * atom_name ) ;

ETERM * get_foo_data_record_from_struct( struct foo_data * s ) ;

ETERM * get_tur_status_atom_from_enum( enum tur_status s ) ;
enum tur_status get_tur_status_enum_from_atom_name( const char * atom_name ) ;



int main()
{

  // Provided by the Seaplus library:
  byte * buffer = start_seaplus_driver() ;

  LOG_TRACE( "Driver started." ) ;

  /* Reads a full command from (receive) buffer, based on its initial length:
   *
   * (a single term is expected hence read)
   *
   */
  while ( read_command( buffer ) > 0 )
  {

	//LOG_TRACE( "New command received." ) ;

	/* Will be set to the corresponding Seaplus-defined function identifier (ex:
	 * whose value is FOO_1_ID):
	 *
	 */
	fun_id current_fun_id ;

	/* Will be set to the number of parameters obtained from Erlang for the
	 * function whose identifier has been transmitted:
	 *
	 */
	arity param_count ;


	// Array containing, in-order, the (param_count) transmitted parameters:
	ETERM ** parameters = NULL ;

	ETERM * call_term = get_function_information( buffer, &current_fun_id,
	  &param_count, &parameters ) ;

	/*
	LOG_DEBUG( "Function identifier is %u, arity is %u.", current_fun_id,
	  param_count ) ;
	 */

	// Now, taking care of the corresponding function call:
	switch( current_fun_id )
	{


	case FOO_1_ID:

	  LOG_DEBUG( "Executing foo/1." ) ;
	  // -spec foo( integer() ) -> integer() vs int foo( int a )

	  check_arity_is( 1, param_count, FOO_1_ID ) ;

	  // So we expect the (single, hence first) parameter to be an integer:
	  int foo_a_param = get_parameter_as_int( 1, parameters ) ;

	  // Actual call:
	  int foo_result = foo( foo_a_param ) ;

	  // Sending of the result:
	  write_as_int( buffer, foo_result ) ;

	  break ;


	case BAR_2_ID:

	  LOG_DEBUG( "Executing bar/2." ) ;

	  /* -spec bar( float(), foo_status() ) -> foo_data() vs
	   * struct foo * bar( double a, enum foo_status status )
	   *
	   */

	  check_arity_is( 2, param_count, BAR_2_ID ) ;

	  // Getting first the Erlang float:
	  double bar_double_param = get_parameter_as_double( 1, parameters ) ;

	  // Then the atom for foo_status():
	  char * atom_name = get_parameter_as_atom( 2, parameters ) ;

	  // Converting said atom for the C API:
	  enum foo_status bar_status_param = get_foo_status_from_atom( atom_name ) ;

	  // Actual call:
	  struct foo_data * struct_res = bar( bar_double_param, bar_status_param ) ;

	  /* Converting this result into a relevant term (that will be deallocated
	   * by next write_term/2:
	   *
	   */
	  ETERM * foo_data_res = get_foo_data_record_from_struct( struct_res ) ;

	  // Sending of the result record:
	  write_term( buffer, foo_data_res ) ;

	  break ;


	case BAZ_2_ID:

	  LOG_DEBUG( "Executing baz/2." ) ;

	  /* -spec baz( integer(), text_utils:ustring() ) -> tur_status() vs
	   * enum tur_status baz( unsigned int u, const char * m )
	   *
	   */

	  check_arity_is( 2, param_count, BAZ_2_ID ) ;

	  // Getting first the (unsigned) integer:
	  int baz_int_param = get_parameter_as_int( 1, parameters ) ;

	  // Then the string:
	  char * baz_string_param = get_parameter_as_string( 2, parameters ) ;

	  // Actual call:
	  enum tur_status enum_res = baz( baz_int_param, baz_string_param ) ;

	  ETERM * tur_status_res = get_tur_status_atom_from_enum( enum_res ) ;

	  // Sending of the result atom:
	  write_term( buffer, tur_status_res ) ;

	  break ;


	case TUR_0_ID:

	  LOG_DEBUG( "Executing tur/0." ) ;

	  // -spec tur() -> bool() vs bool tur()

	  check_arity_is( 0, param_count, TUR_0_ID ) ;

	  // Actual call:
	  bool res = tur() ;

	  // Sending of the result atom:
	  write_as_bool( buffer, res ) ;

	  break ;


	case FROB_1_ID:

	  LOG_DEBUG( "Executing frob/1." ) ;

	  /* frob( tur_status() ) -> text_utils:ustring() vs
	   * char * frob( enum tur_status )
	   *
	   */

	  check_arity_is( 1, param_count, FROB_1_ID ) ;

	  char * tur_atom_name = get_parameter_as_atom( 1, parameters ) ;

	  enum tur_status s = get_tur_status_enum_from_atom_name( tur_atom_name ) ;

	  char * string_res = frob( s ) ;

	  write_as_string( buffer, string_res ) ;

	  break ;


	default:
	  raise_error( "Unknown function identifier: %u", current_fun_id ) ;

	}

	clean_up_command( call_term, parameters ) ;

  }

  stop_seaplus_driver( buffer ) ;

}



// Converts the name of an atom into a foo_status enum.
enum foo_status get_foo_status_from_atom( const char * atom_name )
{

  if ( strcmp( atom_name, "low_speed" ) == 0 )
	return low_speed ;

  if ( strcmp( atom_name, "moderate_speed" ) == 0 )
	return moderate_speed ;

  if ( strcmp( atom_name, "full_speed" ) == 0 )
	return full_speed ;

  raise_error( "Unable to convert atom name '%s' into a foo_status enum.",
	atom_name ) ;

  // To silence compiler:
  return 0 ;

}


/* Returns a foo_data record from specified counterpart struct.
 *
 * We want to return a foo_data record, i.e. a { foo_data, Count:: integer(),
 * Value:: float() } triplet.
 *
 * Note: ownership of the returned term transferred to the caller.
 *
 */
ETERM * get_foo_data_record_from_struct( struct foo_data * s )
{

  // 3 elements:
  ETERM ** tuple_array = (ETERM **) malloc( 3 * sizeof(ETERM *) ) ;

  tuple_array[0] = erl_mk_atom( "foo_data" ) ;
  tuple_array[1] = erl_mk_int( s->count ) ;
  tuple_array[2] = erl_mk_float( (float) s->value ) ;

  ETERM * res = erl_mk_tuple( tuple_array, 3 ) ;

  return res ;

}


/* Returns a tur_status atom from specified enum.
 *
 */
ETERM * get_tur_status_atom_from_enum( enum tur_status s )
{

  if ( s == tur_value )
	return erl_mk_atom( "tur_value" ) ;

  if ( s == non_tur_value )
	return erl_mk_atom( "non_tur_value" ) ;

  raise_error( "Unexpected tur_status enum: %i", s ) ;

  // To silence compiler:
  return NULL ;

}


/* Returns a tur_status enum from specified atom name (as a string).
 *
 */
enum tur_status get_tur_status_enum_from_atom_name( const char * atom_name )
{

  if ( strcmp( atom_name, "tur_value" ) == 0 )
	return tur_value ;

  if ( strcmp( atom_name, "non_tur_value" ) == 0 )
	return non_tur_value ;

  raise_error( "Unexpected tur_status atom name: %s", atom_name ) ;

  // To silence compiler:
  return 0 ;

}
