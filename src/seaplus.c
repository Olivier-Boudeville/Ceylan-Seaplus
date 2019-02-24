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


#include "seaplus.h"


// For read and write:
#include <unistd.h>

// For exit, free:
#include <stdlib.h>

// For strlen:
#include <string.h>

// For fprintf:
#include <stdio.h>

// For vffprintf:
#include <stdarg.h>



/* Obtaining an identifier of the current service instance could allow
 * log files not to step on each other by bearing different names.
 *
 */
const char * default_log_filename = "seaplus.log" ;


const byte_count buffer_size = 4096*8 ;

// Actual file used for logging:
FILE * log_file = NULL ;


#include <stdio.h>


/*
 * Logging is especially useful as drivers, based on C-nodes, are silently run
 * and leave no trace.
 *
 */


void start_logging( const char * log_filename )
{

  if ( log_file == NULL )
  {

	/* Opens a text file for writing, truncating it; if it does not exist,
	 * then a new file is created:
	 */
	log_file = fopen( log_filename, "w" ) ;

	fprintf( log_file, "Starting Seaplus session...\n" ) ;

  }
  else
  {

	fprintf( log_file, "Error: Seaplus session already started.\n" ) ;

  }

}


void stop_logging()
{

  if ( log_file != NULL )
  {

	fprintf( log_file, "Stopping Seaplus session.\n");

	fclose( log_file ) ;

	log_file = NULL ;

  }

}



// Logs specified debug message.
void log_debug( const char * format, ... )
{

  if ( log_file != NULL )
  {

	fprintf( log_file, "[debug] " );

	va_list arg_ptr ;

	va_start( arg_ptr, format ) ;
	vfprintf( log_file, format, arg_ptr ) ;
	va_end( arg_ptr ) ;

	fprintf( log_file, "\n" );

  }

}


// Logs specified trace message.
void log_trace( const char * format, ... )
{

  if ( log_file != NULL )
  {

	fprintf( log_file, "[trace] " );

	va_list arg_ptr ;

	va_start( arg_ptr, format ) ;
	vfprintf( log_file, format, arg_ptr ) ;
	va_end( arg_ptr ) ;

	fprintf( log_file, "\n" );

  }

}


// Raises specified error: reports it in logs, and halts.
void raise_error( const char * format, ... )
{

  if ( log_file != NULL )
  {

	fprintf( log_file, "[error] " );

	va_list arg_ptr ;

	va_start( arg_ptr, format ) ;
	vfprintf( log_file, format, arg_ptr ) ;
	va_end( arg_ptr ) ;

	fprintf( log_file, "\n" );

  }

  exit( EXIT_FAILURE ) ;

}



// Reference function comments to be found in seaplus.h.


/**
 * Starts the C driver.
 *
 * Returns the encoding/decoding buffer.
 *
 */
byte * start_seaplus_driver()
{

  start_logging( default_log_filename ) ;

  log_debug( "Starting the Seaplus C driver, with a buffer of %u bytes.",
			 buffer_size ) ;

  /* Initiating memory handling, always as:
	 (see http://erlang.org/doc/man/erl_eterm.html#erl_init)
   */
  erl_init( NULL, 0 ) ;

  byte * buffer = (byte *) malloc( buffer_size ) ;

  if ( buffer == NULL )
	raise_error( "Buffer allocation failed." ) ;

  return buffer ;

}



/**
 * Stops the C driver.
 *
 */
void stop_seaplus_driver( byte * buffer )
{

  log_debug( "Stopping the Seaplus C driver." ) ;

  free( buffer ) ;

  // No erl_init/2 counterpart.

  stop_logging() ;

}



/**
 * Reads the specified number of bytes from the specified receive buffer.
 *
 * Returns, if positive, the number of bytes read, otherwise an error code.
 *
 * (helper)
 *
 */
byte_count read_exact( byte *buf, byte_count len )
{

  int i, got=0 ;

  do
  {

	// Reading from file descriptor #0:
	if ( ( i = read( 0, buf+got, len-got ) ) <= 0 )
	  return (i) ;

	got += i ;

  } while ( got < len ) ;

  return( len ) ;

}



/**
 * Receives the next command from the port's input file descriptor, and stores
 * it in the specified buffer.
 *
 */
byte_count read_command( byte *buf )
{

  int len ;

  // Two bytes for command length:
  if ( read_exact( buf, 2 ) != 2 )
	raise_error( "Reading of the length of the command buffer failed." ) ;

  len = (buf[0] << 8) | buf[1] ;

  log_debug( "Will read %i bytes.", len ) ;

  if ( len + 2 > buffer_size )
	raise_error( "Read length (%i) is too high (buffer size: %i).",
	  len, buffer_size ) ;

  return read_exact( buf, len ) ;

}



/**
 * Raises an error should the actual arity not be the expected one for specified
 * function.
 *
 */
void check_arity_is( arity expected, arity actual, fun_id id )
{

  if ( expected != actual )
	raise_error( "Expecting arity %u for function identifier %u, "
	  "got %u instead.", expected, id, actual ) ;

}




/* First, accessors to values (getters) returned by erl_decode.
 *
 * Note: visibly, ownership of the result of these decode calls is transferred
 * to the caller, who has thus to deallocate them, which is done in these
 * getters whenever possible.
 *
 */



/// Tuple subsection.

/**
 * Returns the element i of specified tuple, as a term that shall be
 * deallocated.
 *
 */
ETERM * get_element_from_tuple( tuple_index i, ETERM *tuple_term )
{

  ETERM * elem = erl_element( i, tuple_term ) ;

  if ( elem == NULL )
	raise_error( "Reading tuple element at index %u failed.", i ) ;

  return elem ;

}




// Returns the element i of specified tuple, as an int.
signed int get_element_as_int( tuple_index i, ETERM *tuple_term )
{

  ETERM * elem = get_element_from_tuple( i, tuple_term ) ;

  if ( ! ERL_IS_INTEGER( elem ) )
	raise_error( "Tuple element %u cannot be cast to integer.", i ) ;

  int res = ERL_INT_VALUE( elem ) ;

  erl_free_term( elem ) ;

  return res ;

}


// Returns the element i of specified tuple, as an unsigned int.
unsigned int get_element_as_unsigned_int( tuple_index i, ETERM *tuple_term )
{

  ETERM * elem = get_element_from_tuple( i, tuple_term ) ;

  if ( ! ERL_IS_UNSIGNED_INTEGER( elem ) )
	raise_error( "Tuple element %u cannot be cast to unsigned integer.", i ) ;

  unsigned int res = ERL_INT_UVALUE( elem ) ;

  erl_free_term( elem ) ;

  return res ;

}


// Returns the element i of specified tuple, as a double.
double get_element_as_double( tuple_index i, ETERM *tuple_term )
{

  ETERM * elem = get_element_from_tuple( i, tuple_term ) ;

  if ( ! ERL_IS_FLOAT( elem ) )
	raise_error( "Tuple element %u cannot be cast to double.", i ) ;

  double res = ERL_FLOAT_VALUE( elem ) ;

  erl_free_term( elem ) ;

  return res ;

}


/**
 * Returns the element i of specified tuple, as a string (char *).
 *
 * Ownership of the returned string transferred to the caller.
 *
 * Note: cannot return a const char*, as the caller is to deallocate that
 * string.
 *
 */
char * get_element_as_string( tuple_index i, ETERM *tuple_term )
{

  ETERM * elem = get_element_from_tuple( i, tuple_term ) ;

  if ( ! ERL_IS_LIST( elem ) )
	raise_error( "Tuple element %u cannot be cast to string (i.e. list)", i ) ;

  log_debug( "String length is %u bytes.", erl_length( elem ) ) ;

  char * res = erl_iolist_to_string( elem ) ;

  log_debug( "Read string: '%s'.", res ) ;

  // Expected to be null-terminated:
  //char * stringContent = (char *) ERL_BIN_PTR( elem ) ;

  //int size = ERL_BIN_SIZE( elem ) ;

  // Bug somewhere...

  //char * res = (char *) malloc( size * sizeof(char) ) ;

  //strncpy( res, stringContent, size ) ;

  erl_free_term( elem ) ;

  return res ;

}



/// List subsection.


/**
 * Returns the head of the specified, supposedly non-empty, list.
 *
 * Note that the return term shall be freed (with erl_free_term/1) by the
 * caller.
 *
 */
ETERM * get_head( ETERM * list_term )
{

  ETERM * head = erl_hd( list_term ) ;

  if ( head == NULL )
	raise_error( "Unable to get the head of specified list." ) ;

  return head ;

}



/**
 * Returns the head of the specified, supposedly non-empty, list, as an integer.
 *
 */
int get_head_as_int( ETERM * list_term )
{

  ETERM * head_term = get_head( list_term ) ;

   if ( ! ERL_IS_INTEGER( head_term ) )
	raise_error( "Head of list cannot be cast to integer." ) ;

  int res = ERL_INT_VALUE( head_term ) ;

  erl_free_term( head_term ) ;

  return res ;

}



/**
 * Returns the head of the specified, supposedly non-empty, list, as an unsigned
 * integer.
 *
 */
unsigned int get_head_as_unsigned_int( ETERM * list_term )
{

  ETERM * head_term = get_head( list_term ) ;

   if ( ! ERL_IS_UNSIGNED_INTEGER( head_term ) )
	raise_error( "Head of list cannot be cast to unsigned integer." ) ;

  int res = ERL_INT_UVALUE( head_term ) ;

  erl_free_term( head_term ) ;

  return res ;

}



/**
 * Returns the head of the specified, supposedly non-empty, list as a double.
 *
 */
double get_head_as_double( ETERM * list_term )
{

  ETERM * head_term = get_head( list_term ) ;

   if ( ! ERL_IS_FLOAT( head_term ) )
	raise_error( "Head of list cannot be cast to double." ) ;

  double res = ERL_FLOAT_VALUE( head_term ) ;

  erl_free_term( head_term ) ;

  return res ;

}



/**
 * Returns the head of the specified, supposedly non-empty, list as an atom
 * (translated to a char*)
 *
 * Ownership of the returned string transferred to the caller (who shall use
 * erl_free/1 to deallocate it).
 *
 */
char * get_head_as_atom( ETERM * list_term )
{

  ETERM * head_term = get_head( list_term ) ;

  if ( ! ERL_IS_ATOM( head_term ) )
	raise_error( "Head of list cannot be cast to atom." ) ;

  /* The pointer returned by the ERL_ATOM_PTR macro references memory possessed
   * by head_term, that will be freed during this call, so the corresponding
   * string shall be copied:
   *
   */

  char * atom_name = ERL_ATOM_PTR( head_term ) ;

  char * res = strdup( atom_name ) ;

   erl_free_term( head_term ) ;

  return res ;

}



/**
 * Returns the head of the specified, supposedly non-empty, list, as a string.
 *
 * Ownership of the returned string transferred to the caller (who shall use
 * erl_free/1 to deallocate it).
 *
 */
char * get_head_as_string( ETERM * list_term )
{

  ETERM * head_term = get_head( list_term ) ;

  if ( ! ERL_IS_LIST( head_term ) )
	raise_error( "Head of list cannot be cast to list (string)." ) ;

  // Strangely, no ERL_STRING_VALUE or alike in Erl_Interface, so:
  char * res_string = erl_iolist_to_string( head_term ) ;

  if ( res_string == NULL )
	raise_error( "Head of list cannot be converted to string." ) ;

  erl_free_term( head_term ) ;

  return res_string ;

}




/**
 * Returns the tail of the specified, supposedly non-empty, list.
 *
 * Note that the return term shall be freed (with erl_free_term/1) by the
 * caller.
 *
 */
ETERM * get_tail( ETERM * list_term )
{

  ETERM * tail = erl_tl( list_term ) ;

  if ( tail == NULL )
	raise_error( "Unable to get the tail of specified list." ) ;

  return tail ;

}






/* Second, setters of values accepted by erl_encode.
 *
 */


/**
 * Writes specified term into specified buffer.
 *
 * Takes ownership, and deallocates, specified term.
 *
 */
void write_term( byte * buffer, ETERM * term )
{

  // Encodes that ETERM into the specified (send) buffer:
  if ( erl_encode( term, buffer ) == 0 )
	raise_error( "Encoding in buffer failed." ) ;

  int bytes_to_write = erl_term_len( term ) ;

  if ( bytes_to_write == 0 )
	raise_error( "Empty term length." ) ;

  // Sends that buffer:
  write_buffer( buffer, bytes_to_write ) ;

  // Clean-up ETERM memory allocated for this round:
  erl_free_term( term ) ;

}



/**
 * Writes in specified return buffer the specified (signed) integer result.
 *
 */
void write_as_bool( byte * buffer, bool b )
{

  // Constructing the ETERM struct that represents the bool (atom) result:
  ETERM * bool_term ;

  if( b == true )
	bool_term = erl_mk_atom( "true" ) ;
  else
	bool_term = erl_mk_atom( "false" ) ;

  if ( bool_term == NULL )
	raise_error( "Erlang boolean creation failed." ) ;

  write_term( buffer, bool_term ) ;

}



/**
 * Writes in specified return buffer the specified (signed) integer result.
 *
 */
void write_as_int( byte * buffer, int i )
{

  // Constructing the ETERM struct that represents the integer result:
  ETERM * int_term = erl_mk_int( i ) ;

  if ( int_term == NULL )
	raise_error( "Erlang integer creation failed." ) ;

  write_term( buffer, int_term ) ;

}


/**
 * Writes in specified return buffer the specified unsigned integer result.
 *
 */
void write_as_unsigned_int( byte * buffer, unsigned int u )
{

  // Constructing the ETERM struct that represents the unsigned integer result:
  ETERM * uint_term = erl_mk_uint( u ) ;

  if ( uint_term == NULL )
	raise_error( "Erlang unsigned integer creation failed." ) ;

  write_term( buffer, uint_term ) ;

}


/**
 * Writes in specified return buffer the specified string result.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_as_string( byte * buffer, char * string )
{

  // Constructing the ETERM struct that represents the string result:

  size_t len = strlen( string ) ;

  ETERM * string_term = erl_mk_estring( string, len ) ;

  if ( string_term == NULL )
	raise_error( "Erlang string creation failed." ) ;

  write_term( buffer, string_term ) ;

  // Not owned: free( string ) ;

}



/**
 * Writes in specified return buffer the specified binary result.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_as_binary( byte * buffer, char * string )
{

  // Constructing the ETERM struct that represents the string result:

  size_t len = strlen( string ) ;

  ETERM * binary_term = erl_mk_binary( string, len ) ;

  if ( binary_term == NULL )
	raise_error( "Erlang binary creation failed." ) ;

  // Encodes that ETERM into the (send) buffer:
  erl_encode( binary_term, buffer ) ;

  // Sends that buffer:
  write_buffer( buffer, erl_term_len( binary_term ) ) ;

  // Clean-up ETERM memory allocated for this round:
  erl_free_term( binary_term ) ;

  // Not owned: free( string ) ;

}



/**
 * Writes the specified number of bytes to the specified send buffer.
 *
 * Returns, if positive, the number of bytes written, otherwise an error code.
 *
 * (helper)
 *
 */
byte_count write_exact( byte *buf, byte_count len )
{

  int i, wrote = 0 ;

  do
  {

	// Writing to file descriptor #1:
	if ( ( i = write( 1, buf+wrote, len-wrote ) ) <= 0 )
	  return (i) ;

	wrote += i ;

  } while ( wrote < len ) ;

  return ( len ) ;

}



/**
 * Sends the content of the specified buffer through the port's output file
 * descriptor.
 *
 * Returns the number of bytes written.
 *
 */
byte_count write_buffer( byte *buf, byte_count len )
{

  log_debug( "Will write %i bytes.", len ) ;

  if ( len + 2 > len )
  {

	raise_error( "Write length (%i) too high (buffer size: %i).",
	  len, buffer_size ) ;
	return (-1) ;

  }

  byte li;

  // Two bytes for command length:
  li = (len >> 8) & 0xff ;
  write_exact( &li, 1 ) ;

  li = len & 0xff ;
  write_exact( &li, 1 ) ;

  return write_exact( buf, len ) ;

}
