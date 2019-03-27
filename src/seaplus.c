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


// For read and write, get_pid:
#include <unistd.h>

// For time and localtime:
#include <time.h>


// For exit, free:
#include <stdlib.h>

// For strlen:
#include <string.h>

// For fprintf, sprintf:
#include <stdio.h>

// For vffprintf:
#include <stdarg.h>

// For strerror:
#include <errno.h>


const char * default_log_base_filename = "seaplus-driver" ;

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

#if SEAPLUS_ENABLE_LOG

  if ( log_file == NULL )
  {

	/* Opens a text file for writing, truncating it; if it does not exist,
	 * then a new file is created:
	 */
	log_file = fopen( log_filename, "w" ) ;

	// No buffering wanted here:
	setbuf( log_file, NULL ) ;

	log_debug( "Starting Seaplus session..." ) ;

  }
  else
  {

	log_debug( "Error: Seaplus session already started." ) ;

  }

#endif // SEAPLUS_ENABLE_LOG

}


void stop_logging()
{

#if SEAPLUS_ENABLE_LOG

  if ( log_file != NULL )
  {

	log_debug( "Stopping Seaplus session.") ;

	fclose( log_file ) ;

	log_file = NULL ;

  }

#endif // SEAPLUS_ENABLE_LOG

}


#define TIMESTAMP_FORMAT "[%d/%d/%d %d:%02d:%02d]"


// Logs specified debug message.
void log_debug( const char * format, ... )
{

  if ( log_file != NULL )
  {

	time_t t = time( NULL ) ;

	struct tm tm = *localtime( &t ) ;

	fprintf( log_file, TIMESTAMP_FORMAT "[debug] ", tm.tm_year + 1900,
	  tm.tm_mon + 1, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec ) ;

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

	time_t t = time( NULL ) ;

	struct tm tm = *localtime( &t ) ;

	fprintf( log_file, TIMESTAMP_FORMAT "[trace] ", tm.tm_year + 1900,
	  tm.tm_mon + 1, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec ) ;

	va_list arg_ptr ;

	va_start( arg_ptr, format ) ;
	vfprintf( log_file, format, arg_ptr ) ;
	va_end( arg_ptr ) ;

	fprintf( log_file, "\n" );

  }

}


// Logs specified warning message.
void log_warning( const char * format, ... )
{

  if ( log_file != NULL )
  {

	time_t t = time( NULL ) ;

	struct tm tm = *localtime( &t ) ;

	fprintf( log_file, TIMESTAMP_FORMAT "[warning] ", tm.tm_year + 1900,
	  tm.tm_mon + 1, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec ) ;

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

  pid_t current_pid = getpid() ;

  char log_filename[100] ;

  int res = sprintf( log_filename, "%s.%i.log", default_log_base_filename,
	current_pid ) ;

  if ( res < 0 )
	exit( EXIT_FAILURE ) ;

  start_logging( log_filename ) ;

  LOG_DEBUG( "Starting the Seaplus C driver, with a buffer of %u bytes.",
			 buffer_size ) ;

  /* Initiating memory handling, always as:
	 (see http://erlang.org/doc/man/erl_eterm.html#erl_init)
   */
  erl_init( NULL, 0 ) ;

  byte * buffer = (byte *) malloc( buffer_size ) ;

  if ( buffer == NULL )
	raise_error( "Buffer allocation failed." ) ;

#ifdef DEBUG_SEAPLUS

  long unsigned int allocated_count, freed_count ;

  erl_eterm_statistics( &allocated_count , &freed_count ) ;

  LOG_TRACE( "At start-up: currently allocated blocks: %ld; "
	"length of freelist: %ld.", allocated_count, freed_count ) ;

#endif // DEBUG_SEAPLUS

  return buffer ;

}



// Performs housekeeping after a command has been executed.
void clean_up_command( ETERM * call_term, ETERM ** parameters )
{

#ifdef DEBUG_SEAPLUS

	long unsigned int allocated_count, freed_count ;

#endif // DEBUG_SEAPLUS

	/*
	erl_eterm_statistics( &allocated_count, &freed_count ) ;

	LOG_TRACE( "Before term release: currently allocated blocks: %ld; "
	  "length of freelist: %ld.", allocated_count, freed_count ) ;

	erl_eterm_release() ;


	erl_eterm_statistics( &allocated_count, &freed_count ) ;

	LOG_TRACE( "After term release: currently allocated blocks: %ld; "
	  "length of freelist: %ld.", allocated_count, freed_count ) ;

	*/

	erl_free_compound( call_term ) ;

	/*
	erl_eterm_statistics( &allocated_count, &freed_count ) ;

	LOG_TRACE( "After free: currently allocated blocks: %ld; "
	  "length of freelist: %ld.", allocated_count, freed_count ) ;

	*/

	erl_eterm_release() ;

#ifdef DEBUG_SEAPLUS

	erl_eterm_statistics( &allocated_count, &freed_count ) ;

	/*
	LOG_TRACE( "After second term release: currently allocated blocks: %ld; "
	  "length of freelist: %ld.", allocated_count, freed_count ) ;
	*/

	if ( allocated_count != 0 )
	  log_warning( "At command clean-up, still %lu allocated blocks.",
		allocated_count ) ;

#endif // DEBUG_SEAPLUS

	free( parameters ) ;

}



/**
 * Stops the C driver.
 *
 */
void stop_seaplus_driver( byte * buffer )
{

  LOG_DEBUG( "Stopping the Seaplus C driver." ) ;

  free( buffer ) ;

#ifdef DEBUG_SEAPLUS

  long unsigned int allocated_count, freed_count ;

  erl_eterm_statistics( &allocated_count , &freed_count ) ;

  LOG_TRACE( "At stop: currently allocated blocks: %ld; "
	"length of freelist: %ld.", allocated_count, freed_count ) ;

  if ( allocated_count != 0 )
	log_warning( "At stop, still %lu allocated blocks.", allocated_count ) ;

 #endif // DEBUG_SEAPLUS

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

  byte_count got = 0 ;

  do
  {

	// Reading from file descriptor #0:
	byte_count i = read( 0, buf+got, len-got ) ;

	if ( i <= 0 )
	{

	  // Possibly 0 byte if the port has been closed in the meantime:
	  if ( i == 0 )
		return 0 ;

	  // Expected to be -1 (error) then:
	  raise_error( "Reading from buffer failed: %s.", strerror( errno ) ) ;

	}

	got += i ;

  } while ( got < len ) ;

  //LOG_DEBUG( "Read %i bytes.", len ) ;

  return len ;

}



/**
 * Receives the next command from the port's input file descriptor, and stores
 * it in the specified buffer.
 *
 */
byte_count read_command( byte *buf )
{

  // Two bytes for command length:
  byte_count len = read_exact( buf, 2 ) ;

  // Not reading these length bytes may not be abnormal, if stopping:
  if ( len == 2 )
  {

	len = (buf[0] << 8) | buf[1] ;

	//LOG_DEBUG( "Will read %i bytes.", len ) ;

	if ( len + 2 > buffer_size )
	  raise_error( "Read length (%i) is too high (buffer size: %i).",
		len, buffer_size ) ;

	return read_exact( buf, len ) ;

  }
  else
  {

	if ( len == 0 )
	{
	  // The port must have been closed:
	  return 0 ;
	}
	else
	{

	  raise_error( "Reading of the length of the command buffer failed "
		"(read %i bytes).", len ) ;

	  // Silence compiler:
	  return 0 ;

	}

  }

}


/**
 * Determines, from specified buffer, the information regarding the
 * (Erlang-side) specified function, namely its function identifier and its
 * parameters, set in the variables whose reference is specified.
 *
 * Returns a term that is to be deallocated once all parameters will have been
 * used.
 *
 */
ETERM * get_function_information( byte * buffer, fun_id * current_fun_id,
  arity * param_count, ETERM *** parameters )
{

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
	  raise_error( "Read term is not a tuple (whereas pair expected)." ) ;

	tuple_size tuple_s = ERL_TUPLE_SIZE( read_pair ) ;

	if ( tuple_s != 2 )
	  raise_error( "Read tuple is not a pair (%u elements found).",
		tuple_s ) ;


	/* Gets the first element of the pair, i.e. the Seaplus-defined function
	 * identifier (ex: whose value is FOO_1_ID):
	 *
	 */
	*current_fun_id = get_element_as_int( 1, read_pair ) ;

	//LOG_DEBUG( "Reading command: function identifier is %i.", *current_fun_id ) ;


	/* Second element of the pair is the list of the call parameters (hence the
	 * arity of the function to be called can be checked):
	 *
	 */
	ETERM * cmd_params = get_element_from_tuple( 2, read_pair ) ;

	if( ! ERL_IS_LIST( cmd_params ) )
	  raise_error( "Second element of the parameter pair cannot be cast "
		"to a list." ) ;

	// The number of elements in the list of call parameters:
	arity fun_param_count = erl_length( cmd_params ) ;

	if ( fun_param_count == -1 )
	  raise_error( "Improper list received." ) ;

	/*
	LOG_DEBUG( "%u parameter(s) received for this function.",
	  fun_param_count ) ;
	 */

	*param_count = fun_param_count ;

	/* We used to return the FunParams list as it was, but iterating on it was
	 * making the driver more complex than needed.
	 *
	 * Now we return a simple array of terms (more convenient for the driver
	 * developer, and inline with how NIFs proceed).
	 *
	 */
	ETERM ** fun_params =
	  (ETERM **) malloc( fun_param_count * sizeof( ETERM * ) ) ;

	if ( fun_params == NULL )
	  raise_error( "Creation of parameter array failed." ) ;

	for ( arity i = 0; i < fun_param_count ; i++ )
	{
	  fun_params[ i ] = get_head( cmd_params ) ;
	  cmd_params = get_tail( cmd_params ) ;
	}

	*parameters = fun_params ;

	// Returned to be deallocated as a whole afterwards:
	return read_pair ;

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
	raise_error( "Tuple element %i cannot be cast to integer.", i ) ;

  int res = ERL_INT_VALUE( elem ) ;

  //LOG_DEBUG( "Read integer %i.", res ) ;

  erl_free_term( elem ) ;

  return res ;

}



/**
 * Returns the element i of specified tuple, as an unsigned integer.
 *
 * Note: apparently, Erlang integers are rather returned as 'int', not 'unsigned
 * int'.
 *
 */
unsigned int get_element_as_unsigned_int( tuple_index i, ETERM *tuple_term )
{

  ETERM * elem = get_element_from_tuple( i, tuple_term ) ;

  if ( ! ERL_IS_UNSIGNED_INTEGER( elem ) )
	raise_error( "Tuple element %u cannot be cast to unsigned integer.", i ) ;

  unsigned int res = ERL_INT_UVALUE( elem ) ;

  erl_free_term( elem ) ;

  //LOG_DEBUG( "Read unsigned integer %u.", res ) ;

  return res ;

}


// Returns the element i of specified tuple, as a double.
double get_element_as_double( tuple_index i, ETERM *tuple_term )
{

  ETERM * elem = get_element_from_tuple( i, tuple_term ) ;

  if ( ! ERL_IS_FLOAT( elem ) )
	raise_error( "Tuple element %u cannot be cast to double.", i ) ;

  double res = ERL_FLOAT_VALUE( elem ) ;

  //LOG_DEBUG( "Read double %e.", res ) ;

  erl_free_term( elem ) ;

  return res ;

}


// char * get_element_as_atom( tuple_index i, ETERM *tuple_term ) is lacking.


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

  //LOG_DEBUG( "String length is %u bytes.", erl_length( elem ) ) ;

  char * res = erl_iolist_to_string( elem ) ;

  //LOG_DEBUG( "Read string: '%s'.", res ) ;

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

  //LOG_DEBUG( "Read double %e.", res ) ;

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

  if ( atom_name == NULL )
	raise_error( "Head of list cannot be converted to atom." ) ;

  //LOG_DEBUG( "Read head as atom '%s'.", atom_name ) ;

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

  //LOG_DEBUG( "Read head as string '%s'.", res_string ) ;

  erl_free_term( head_term ) ;

  return res_string ;

}




// Parameter getters.



/**
 * Returns the element at specified index of specified array of parameters,
 * supposed to be an integer.
 *
 * Note: the corresponding terms is not freed, as the parameter array is
 * expected to be deallocated as a whole (recursively) afterwards.
 *
 */
int get_parameter_as_int( parameter_index index, ETERM ** parameters )
{

 // Starts at zero:
  ETERM * elem  = parameters[ index - 1 ] ;

  if ( ! ERL_IS_INTEGER( elem ) )
	raise_error( "Parameter element of index %i cannot be cast to integer.",
	  index ) ;

  int res = ERL_INT_VALUE( elem ) ;

  //LOG_DEBUG( "Read integer parameter %i.", res ) ;

  return res ;

}


/**
 * Returns the element at specified index of specified array of parameters,
 * supposed to be an unsigned integer.
 *
 * Note: the corresponding terms is not freed, as the parameter array is
 * expected to be deallocated as a whole (recursively) afterwards.
 *
 */
unsigned int get_parameter_as_unsigned_int( parameter_index index,
  ETERM ** parameters )
{

  // Starts at zero:
  ETERM * elem  = parameters[ index - 1 ] ;

  if ( ! ERL_IS_UNSIGNED_INTEGER( elem ) )
	raise_error( "Parameter element of index %i cannot be cast to "
	  "unsigned integer.", index ) ;

  int res = ERL_INT_UVALUE( elem ) ;

  //LOG_DEBUG( "Read unsigned integer parameter %u.", res ) ;

  return res ;

}



/**
 * Returns the element at specified index of specified array of parameters,
 * supposed to be an Erlang float (hence returning a C double).
 *
 * Note: the corresponding terms is not freed, as the parameter array is
 * expected to be deallocated as a whole (recursively) afterwards.
 *
 */
double get_parameter_as_double( parameter_index index, ETERM ** parameters )
{

   // Starts at zero:
  ETERM * elem  = parameters[ index - 1 ] ;

  if ( ! ERL_IS_FLOAT( elem ) )
	raise_error( "Parameter element of index %i cannot be cast to double.",
	  index ) ;

  int res = ERL_FLOAT_VALUE( elem ) ;

  //LOG_DEBUG( "Read double parameter %e.", res ) ;

  return res ;

}


/**
 * Returns the element at specified index of specified array of parameters,
 * supposed to be an atom, translated to a char*.
 *
 * Note: the corresponding term is not freed, as the parameter array is expected
 * to be deallocated as a whole (recursively) afterwards.
 *
 * Ownership of the returned string transferred to the caller (who shall use
 * erl_free/1 to deallocate it).
 *
 */
char * get_parameter_as_atom( parameter_index index, ETERM ** parameters )
{

  // Starts at zero:
  ETERM * elem  = parameters[ index - 1 ] ;

  if ( ! ERL_IS_ATOM( elem ) )
	raise_error( "Parameter element of index %i cannot be cast to atom.",
	  index ) ;

  char * res = ERL_ATOM_PTR( elem ) ;

  //LOG_DEBUG( "Read atom '%s'.", res ) ;

  return res ;

}



/**
 * Returns the element at specified index of specified array of parameters,
 * supposed to be a string.
 *
 * Note: the corresponding terms is not freed, as the parameter array is
 * expected to be deallocated as a whole (recursively) afterwards.
 *
 * Ownership of the returned string transferred to the caller (who shall use
 * erl_free/1 to deallocate it).
 *
 */
char * get_parameter_as_string( parameter_index index, ETERM ** parameters )
{

  // Starts at zero:
  ETERM * elem  = parameters[ index - 1 ] ;

  if ( ! ERL_IS_LIST( elem ) )
	raise_error( "Parameter element of index %i cannot be cast to string "
	  "(i.e. list)", index ) ;

  //LOG_DEBUG( "String length is %u bytes.", erl_length( elem ) ) ;

  char * res = erl_iolist_to_string( elem ) ;

  //LOG_DEBUG( "Read string: '%s'.", res ) ;

  return res ;

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
  // Not 'erl_free_term( term ) ;', as of course we may write compounds:
  erl_free_compound( term ) ;

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
 * Writes in specified return buffer the specified double result.
 *
 */
void write_as_double( byte * buffer, double d )
{

  // Constructing the ETERM struct that represents the unsigned integer result:
  ETERM * float_term = erl_mk_float( d ) ;

  if ( float_term == NULL )
	raise_error( "Erlang float creation failed." ) ;

  write_term( buffer, float_term ) ;

}


/**
 * Writes in specified return buffer the specified string result.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_as_string( byte * buffer, const char * string )
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
void write_as_binary( byte * buffer, const char * string )
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

  byte_count wrote = 0 ;

  do
  {

	byte_count i = write( 1, buf+wrote, len-wrote ) ;

	// Writing to file descriptor #1:
	if ( i <= 0 )
	  return i ;

	wrote += i ;

  } while ( wrote < len ) ;

  return len ;

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

  //LOG_DEBUG( "Will write %i bytes.", len ) ;

  if ( len + 2 > buffer_size )
  {

	raise_error( "Write length (%i) too high (buffer size: %i).",
	  len, buffer_size ) ;

	return -1 ;

  }

  byte li ;

  // Two bytes for command length:
  li = (len >> 8) & 0xff ;
  write_exact( &li, 1 ) ;

  li = len & 0xff ;
  write_exact( &li, 1 ) ;

  return write_exact( buf, len ) ;

}



/**
 * Returns a binary string for specified (NULL-terminated) C string.
 *
 * Does not take ownership of the specified C string.
 *
 * (lacking in a direct form in
 * http://erlang.org/doc/man/erl_eterm.html#erl_mk_binary)
 *
 */
ETERM * make_bin_string( const char * c_string )
{

  return erl_mk_binary( c_string, strlen( c_string ) ) ;

}
