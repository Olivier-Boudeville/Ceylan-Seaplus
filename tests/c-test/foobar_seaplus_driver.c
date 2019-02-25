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
 * The messages from and to Erlang arrives as { FunId, FunParams } pairs, where
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
enum tur_status get_tur_status_enum_from_atom( const char * atom_name ) ;



int main()
{

  // Provided by the Seaplus library:
  byte * buffer = start_seaplus_driver() ;

  log_trace( "Driver started." ) ;


  /* Reads a full command from (receive) buffer, based on its initial length:
   *
   * (a single term is expected hence read)
   *
   */
  while ( read_command( buffer ) > 0 )
  {

	log_trace( "New command received." ) ;

	/*
	 * Reads a { FunId, FunParams } pair thanks to Erl_Interface:
	 *
	 * (see http://erlang.org/doc/man/erl_marshal.html#erl_decode)
	 *
	 * A single term (pair) is expected here:
	 *
	 */
	ETERM * read_pair = erl_decode( buffer ) ;

	if ( read_pair == NULL )
		raise_error(
		  "Decoding of the pair command from the receive buffer failed." ) ;

	if ( ! ERL_IS_TUPLE( read_pair ) )
	  raise_error( "Read term is not a tuple" ) ;

	tuple_size tuple_s = ERL_TUPLE_SIZE( read_pair ) ;

	if( tuple_s != 2 )
	  raise_error( "Read tuple is not a pair (%u elements found).",
		tuple_s ) ;


	/* Gets the first element of the pair, i.e. the Seaplus-defined function
	 * identifier (ex: whose value is FOO_1_ID):
	 *
	 */
	fun_id current_fun_id = get_element_as_unsigned_int( 1, read_pair ) ;

	log_debug( "Reading command: function identifier is %u.", current_fun_id ) ;

	/* Now reading the second element of the pair, supposed to be the list of
	 * the call parameters:
	 *
	 */

	/* Second one is the list of command parameters (hence the arity of the
	 * function to be called can be checked):
	 *
	 */
	ETERM * cmd_params = get_element_from_tuple( 2, read_pair ) ;

	if( ! ERL_IS_LIST( cmd_params ) )
	  raise_error( "Second element of the parameter pair cannot be cast "
		"to a list." ) ;

	// The number of elements in the list of call parameters:
	list_size param_count = erl_length( cmd_params ) ;

	if ( param_count == -1 )
	  raise_error( "Improper list received." ) ;

	log_debug( "%u parameters received for this function.", param_count ) ;


	/* Now, take care of the corresponding function call:
	 *
	 */
	switch( current_fun_id )
	{


	case FOO_1_ID:

	  // -spec foo( integer() ) -> integer() vs int foo( int a )

	  check_arity_is( 1, param_count, FOO_1_ID ) ;

	  // So we expect the parameter list to contain a single integer:
	  int foo_a_param = get_head_as_int( cmd_params ) ;

	  // Actual call:
	  int foo_result = foo( foo_a_param ) ;

	  // Sending of the result:
	  write_as_int( buffer, foo_result ) ;

	  break ;


	case BAR_2_ID:

	  /* -spec bar( float(), foo_status() ) -> foo_data() vs
	   * struct foo * bar( double a, enum foo_status status )
	   *
	   */

	  check_arity_is( 2, param_count, BAR_2_ID ) ;

	  // Getting first the float:
	  double bar_double_param = get_head_as_double( cmd_params ) ;

	  ETERM * cmd_params = get_tail( cmd_params ) ;

	  // Then the atom for foo_status():
	  char * atom_name = get_head_as_atom( cmd_params ) ;

	  enum foo_status bar_status_param = get_foo_status_from_atom( atom_name ) ;

	  // Actual call:
	  struct foo_data * struct_res = bar( bar_double_param, bar_status_param ) ;

	  ETERM * foo_data_res = get_foo_data_record_from_struct( struct_res ) ;

	  // Sending of the result record:
	  write_term( buffer, foo_data_res ) ;

	  break ;


	case BAZ_2_ID:

	  /* -spec baz( integer(), text_utils:ustring() ) -> tur_status() vs
	   * enum tur_status baz( unsigned int u, const char * m )
	   *
	   */

	  check_arity_is( 2, param_count, BAZ_2_ID ) ;

	  // Getting first the (unsigned) integer:
	  unsigned int baz_int_param = get_head_as_unsigned_int( cmd_params ) ;

	  // Then the string:
	  cmd_params = get_tail( cmd_params ) ;

	  char * baz_string_param = get_head_as_string( cmd_params ) ;

	  // Actual call:
	  enum tur_status enum_res = baz( baz_int_param, baz_string_param ) ;

	  ETERM * tur_status_res = get_tur_status_atom_from_enum( enum_res ) ;

	  // Sending of the result atom:
	  write_term( buffer, tur_status_res ) ;

	  break ;


	case TUR_0_ID:

	  // -spec tur() -> bool() vs bool tur()

	  check_arity_is( 0, param_count, TUR_0_ID ) ;

	  // Actual call:
	  bool res = tur() ;

	  // Sending of the result atom:
	  write_as_bool( buffer, res ) ;

	  break ;


	case FROB_1_ID:

	  /* frob( tur_status() ) -> text_utils:ustring() vs
	   * char * frob( enum tur_status )
	   *
	   */

	  check_arity_is( 1, param_count, FROB_1_ID ) ;

	  char * tur_atom_name = get_head_as_atom( cmd_params ) ;

	  enum tur_status s = get_tur_status_enum_from_atom( tur_atom_name ) ;

	  char * string_res = frob( s ) ;

	  write_as_string( buffer, string_res ) ;

	  break ;

	default:
	  raise_error( "Unknown function identifier: %u", current_fun_id ) ;

	}

	erl_free_compound( read_pair ) ;

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


/* Returns a tur_status enum from specified atom.
 *
 */
enum tur_status get_tur_status_enum_from_atom( const char * atom_name )
{

  if ( strcmp( atom_name, "tur_value" ) == 0 )
	return tur_value ;

  if ( strcmp( atom_name, "non_tur_value" ) == 0 )
	return non_tur_value ;

  raise_error( "Unexpected tur_status atom name: %s", atom_name ) ;

  // To silence compiler:
  return 0 ;

}
