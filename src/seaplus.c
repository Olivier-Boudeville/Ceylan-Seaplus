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


/* Current size of the input buffer (possibly increased during the processing),
 * used for decoding input parameters:
 *
 */
// If wanting to test buffer expansion:
//byte_count input_buffer_size = 4 ;
byte_count input_buffer_size = 4096*8 ;



// Default upper bound of string sizes:
const byte_count string_size = 150 ;


// For reading and writing terms:
input_buffer buffer = NULL ;

// Actual file used for logging:
FILE * log_file = NULL ;


/**
 * Tells whether a list (ex: the top-level parameter one, or one that is nested
 * in it) was wrongly encoded by term_to_binary/1 as a string (a series of
 * bytes, i.e. 8-bit characters) instead of a real list happening to contain
 * only small integers; see
 * http://erlang.org/doc/man/ei.html#ei_decode_list_header for more details.
 *
 * This "simple" scheme will work as string-encoded lists cannot be nested (so
 * there is up to one level of them).
 *
 * Note that, afterwards, for that command, only read_int_parameter/2 will read
 * its data from that list-as-a-string.
 *
 */
bool encoded_as_string ;


/**
 * The temporary string whence list element will be read, should an unintended
 * string-encoding of a list have been done:
 *
 */
char * encoded_string = NULL ;


/**
 * The index within a list encoded as a string corresponding to the next
 * (integer) element that will be read:
 *
 */
buffer_index encoded_index ;


/// The number of elements in any current list encoded as a string.
list_size encoded_length ;


#include <stdio.h>


// Forward references:

void check_encoded_list() ;

char * interpret_type_at( const input_buffer buffer,
  const buffer_index * index ) ;


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
 * Returns the (plain) input buffer (for parameter decoding).
 *
 */
void start_seaplus_driver( input_buffer buf )
{

  pid_t current_pid = getpid() ;

  char log_filename[100] ;

  int res = sprintf( log_filename, "%s.%i.log", default_log_base_filename,
	current_pid ) ;

  if ( res < 0 )
	exit( EXIT_FAILURE ) ;

  start_logging( log_filename ) ;

  LOG_DEBUG( "Starting the Seaplus C driver, with an input buffer of %u bytes.",
			 input_buffer_size ) ;

  ei_error error = ei_init() ;

  if ( error != 0 )
	raise_error( "The ei service could not be successfully initialized: %s.",
	  strerror( error ) ) ;

  *buf = (byte *) malloc( input_buffer_size ) ;

  if ( *buf == NULL )
	raise_error( "Allocation of the input buffer failed." ) ;

}



/**
 * Stops the C Seaplus driver.
 *
 */
void stop_seaplus_driver( input_buffer input_buffer )
{

  LOG_DEBUG( "Stopping the Seaplus C driver." ) ;

  if ( encoded_string != NULL )
  {
	  free( encoded_string ) ;
	  encoded_string = NULL ;

  }

  free( *input_buffer ) ;
  *input_buffer = NULL ;

  // No ei_init/0 counterpart found.

  stop_logging() ;

}



/**
 * Initializes the main smart buffer (the internal fields thereof) according to
 * the Erlang binary format.
 *
 */
void init_output_buffer( output_buffer * sm_buf )
{

  if ( ei_x_new_with_version( sm_buf ) != 0 )
	raise_error( "Initialization of smart buffer failed." ) ;

}



// Clears the specified smart buffer (the internal fields thereof).
void clear_output_buffer( output_buffer * sm_buf )
{

 if ( ei_x_free( sm_buf ) != 0 )
	raise_error( "Clearing of smart buffer failed." ) ;

}



/**
 * Prepares for the encoding of the next upcoming command.
 *
 * A (smart) buffer is specified, as in some cases (ex: interrupt handling)
 * multiple output buffers may be useful.
 *
 */
void prepare_for_command( output_buffer * output_sm_buf )
{

  /* finalize_command/1 is expected to have already freed appropriately the
   * internals of that smart buffer:
   *
   */
  init_output_buffer( output_sm_buf ) ;

}



/**
 * Finalizes the current command, supposed to have performed a writing (which is
 * by far the most general case).
 *
 */
void finalize_command_after_writing( output_buffer * output_sm_buf )
{

  write_buffer( output_sm_buf ) ;

  clear_output_buffer( output_sm_buf ) ;

}



/**
 * Reads the specified number of bytes from the specified receive buffer.
 *
 * Returns, if positive, the number of bytes read, otherwise an error code.
 *
 * (helper)
 *
 */
byte_count read_exact( byte * buf, byte_count len )
{

  byte_count got = 0 ;

  LOG_DEBUG( "%d bytes to read.", len ) ;

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
	  raise_error( "Reading from buffer %p at position %i "
		"for a count of %i bytes failed: %s.", buf, got, len-got,
		strerror( errno ) ) ;

	}

	LOG_DEBUG( "%i bytes actually read.", len ) ;

	got += i ;

  } while ( got < len ) ;

  LOG_DEBUG( "Read %i bytes.", len ) ;

  return len ;

}



/**
 * Receives the next command from the port's input file descriptor, and stores
 * it in the specified buffer.
 *
 */
byte_count read_command( input_buffer buf )
{

  LOG_DEBUG( "Reading a new command, from address %p.", buf ) ;

  // Two bytes for command length:
  byte_count len = read_exact( *buf, 2 ) ;

  // Not reading these length bytes may not be abnormal, if stopping:
  if ( len == 2 )
  {


	// Beware to infamous signed chars and negative lengths:
	len = ( ( ((unsigned char *) (*buf))[0] ) << 8)
	  | ( ((unsigned char *) (*buf))[1] ) ;

	LOG_DEBUG( "Command payload to read: %d bytes.", len ) ;

#ifdef DEBUG_SEAPLUS
	if ( len < 0 )
	  raise_error( "Invalid length to read (%d bytes).", len ) ;
#endif

	// Not len + 2, we restart at the beginning of the buffer:
	if ( len > input_buffer_size )
	{

	  /* Better to resize than:

	  raise_error( "Read length (%i) is too high (buffer size: %i).",
		len, input_buffer_size ) ;

	  */

	  LOG_DEBUG( "Expanding input buffer from %i bytes to %i.",
		input_buffer_size, len ) ;

	  // May involve a useless copy as will be overwritten:
	  byte * tmp = (byte *) realloc( *buf, len ) ;

	  if (tmp == NULL)
	  {

		raise_error( "Could not expand input buffer from %i bytes to %i.",
		  input_buffer_size, len ) ;

	  }
	  else
	  {

		*buf = tmp ;
		input_buffer_size = len ;

	  }

	}

	return read_exact( *buf, len ) ;

  }
  else
  {

	if ( len == 0 )
	{

	  LOG_DEBUG( "No byte read, port expected to be closed." ) ;
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
 * parameters, set in the variables whose reference (pointer) is specified.
 *
 */
void read_function_information( input_buffer input_buffer, buffer_index * index,
  fun_id * current_fun_id, arity * param_count )
{

	LOG_TRACE( "Getting function information." ) ;

	int format_version ;

	/* According to
	 * https://erlangcentral.org/wiki/How_to_use_ei_to_marshal_binary_terms_in_port_programs,
	 * the first token in a binary term is the version magic number for the
	 * Erlang binary term format (131, at the time of this writing).
	 *
	 */
	if ( ei_decode_version( *input_buffer, index, &format_version ) != 0 )
	  raise_error( "The version magic number of the binary term format could "
		"not be successfully decoded." ) ;

	LOG_DEBUG( "Read Erlang binary term format version number: %i, "
	  "from index %i.", format_version, *index ) ;

	/*
	 * Reads a { FunId, FunParams } pair (ex: { 5, [ 140, true ] }) thanks to,
	 * now, ei:
	 *
	 * (see http://erlang.org/doc/man/ei.html)
	 *
	 * A single term (pair) is expected here:
	 *
	 */

  tuple_size tuple_s ;

  if ( ei_decode_tuple_header( *input_buffer, index, &tuple_s ) != 0 )
	raise_error( "The function pair could not be successfully decoded. "
	  "Detected type: %s.", interpret_type_at( input_buffer, index ) ) ;

  if ( tuple_s != 2 )
	raise_error( "Unexpected size of function tuple: not a pair, "
	  "%i elements to decode.", tuple_s ) ;


	/* Gets the first element of the pair, i.e. the Seaplus-defined function
	 * identifier (ex: whose value is FOO_1_ID):
	 *
	 */
  if ( ei_decode_long( *input_buffer, index, current_fun_id ) != 0 )
	raise_error( "The function identifier could not be successfully "
	  "decoded. Detected type: %s.",
	  interpret_type_at( input_buffer, index ) ) ;

  LOG_DEBUG( "Reading command: function identifier is %i (index is %i).",
	*current_fun_id, *index ) ;

  /* Second element of the pair is the list of the call parameters (hence the
   * arity of the function to be called can be checked):
   *
   */
  read_list_header_parameter( input_buffer, index, param_count ) ;

  LOG_DEBUG( "%u parameter(s) received for this function.", *param_count ) ;

  /* The (*param_count)+1 elements follow in the input buffer, starting from the
   * current updated index (the last one is the tail of the list, normally an
   * empty list).
   *
   * The handler for the corresponding function identifier is now to read in
   * turn its parameters.
   *
   */

  LOG_TRACE( "Function information obtained." ) ;

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




// Accessors to (parameter) values (getters).


/**
 * Returns the element at current buffer location, supposed to be a (long,
 * signed) integer.
 *
 */
long read_int_parameter( input_buffer decode_buffer, buffer_index * index )
{

  long res ;

  // Default, normal case:
  if ( ! encoded_as_string )
  {

	LOG_DEBUG( "Reading long integer at index %i.", *index ) ;

	ei_error error = ei_decode_long( *decode_buffer, index, &res ) ;

	if ( error != 0 )
	  raise_error( "Parameter at index %i cannot be decoded to (long) integer: "
		"%s; its actual type is %s.", index, strerror( error ),
		interpret_type_at( decode_buffer, index ) ) ;

	LOG_DEBUG( "Read (long, signed) integer parameter %ld.", res ) ;

	return res ;

  }
  else
  {

	// Here we have an incorrect encoding ([0..255] became string()):
	res = (long) encoded_string[ encoded_index ] ;

	encoded_index++ ;

	if ( encoded_index == encoded_length )
	{

	  LOG_DEBUG( "Read last small integer %ld of string-based list "
		"(of size %i).", res, encoded_length ) ;

	  free( encoded_string ) ;
	  encoded_string = NULL ;

	  encoded_as_string = false ;

	}
	else
	{

	  // Elements start at 1 here:
	  LOG_DEBUG( "Read small integer %ld of string-based list (element %i/%i).",
		res, encoded_index, encoded_length ) ;

	}

	return res ;

  }

}



/**
 * Returns the element at current buffer location, supposed to be a (long,
 * unsigned) integer.
 *
 */
unsigned long read_unsigned_int_parameter( input_buffer decode_buffer,
  buffer_index * index )
{

  check_encoded_list() ;

	unsigned long res ;

  ei_error error = ei_decode_ulong( *decode_buffer, index, &res ) ;

  if ( error != 0 )
	raise_error( "Parameter at index %i cannot be decoded to (long) "
	  "unsigned integer: %s; its actual type is %s.", index, strerror( error ),
	  interpret_type_at( decode_buffer, index ) ) ;

  LOG_DEBUG( "Read (long, unsigned) integer parameter %ul.", res ) ;

  return res ;

}



/**
 * Returns the element at current buffer location, supposed to be a double.
 *
 */
double read_double_parameter( input_buffer decode_buffer,
  buffer_index * index )
{

  check_encoded_list() ;

  double res ;

   ei_error error = ei_decode_double( *decode_buffer, index, &res ) ;

  if ( error != 0 )
	raise_error( "Parameter at index %i cannot be decoded to double: %s; "
	  "its actual type is %s.", index, strerror( error ),
	  interpret_type_at( decode_buffer, index ) ) ;

  LOG_DEBUG( "Read double parameter %lf.", res ) ;

  return res ;

}



/**
 * Returns the element at current buffer location, supposed to be an atom,
 * translated to a char*, whose ownership is transferred to the caller (who is
 * thus supposed to deallocate it ultimately, with standard free/1).
 *
 */
char * read_atom_parameter( input_buffer decode_buffer, buffer_index * index )
{

 check_encoded_list() ;

 char * res = (char*) malloc( MAXATOMLEN * sizeof( char ) ) ;

  if ( res == NULL )
	raise_error( "Failed to allocate atom buffer." ) ;

  ei_error error = ei_decode_atom( *decode_buffer, index, res ) ;

  if ( error != 0 )
	raise_error( "Parameter at index %i cannot be decoded to atom: %s; "
	  "its actual type is %s.", index, strerror( error ),
	  interpret_type_at( decode_buffer, index ) ) ;

  LOG_DEBUG( "Read atom parameter '%s'.", res ) ;

  return res ;

}



/**
 * Returns the element at current buffer location, supposed to be a (plain)
 * string, whose ownership is transferred to the caller (who is thus supposed to
 * deallocate it ultimately, with standard free/1)..
 *
 */
char * read_string_parameter( input_buffer decode_buffer, buffer_index * index )
{

  check_encoded_list() ;

  int type ;
  int size ;

  if ( ei_get_type( *decode_buffer, index, &type, &size ) != 0 )
	raise_error( "Cannot determine the type encoded at index %i", index ) ;

  if ( type != ERL_STRING_EXT )
	raise_error( "Not a string at index %i, got %s.", index,
	  interpret_type_at( decode_buffer, index ) ) ;

  // For the null terminator:
  char * res = (char*) malloc( (size+1) * sizeof( char ) ) ;

  if ( res == NULL )
	raise_error( "Failed to allocate string buffer." ) ;

  if ( ei_decode_string( *decode_buffer, index, res ) != 0 )
	raise_error( "Parameter at index %i cannot be decoded to string; "
	  "its actual type is %s.", *index,
	  interpret_type_at( decode_buffer, index ) ) ;

  LOG_DEBUG( "Read (plain) string: '%s'.", res ) ;

  return res ;

}



/**
 * Returns the element at current buffer location, supposed to be a binary,
 * returned as a string whose ownership is transferred to the caller (who is
 * thus supposed to deallocate it ultimately, with standard free/1).
 *
 */
char * read_binary_parameter( input_buffer decode_buffer, buffer_index * index )
{

  check_encoded_list() ;

  int type ;
  int size ;

  ei_error error = ei_get_type( *decode_buffer, index, &type, &size ) ;

  if ( error != 0 )
	raise_error( "Cannot determine the type encoded at index %i: %s.",
	  index, strerror( error ) ) ;

  if ( type != ERL_BINARY_EXT )
	raise_error( "Not a binary at index %i, got %s.", index,
	  interpret_type_at( decode_buffer, index ) ) ;

  // No null terminator here:
  char * res = (char*) malloc( size * sizeof( char ) ) ;

  if ( res == NULL )
	raise_error( "Failed to allocate binary buffer." ) ;

  // Not used:
  long l_size ;

  if ( ei_decode_binary( *decode_buffer, index, res, &l_size ) != 0 )
	raise_error( "Parameter at index %i cannot be decoded to binary: %s; "
	  "its actual type is %s.", index, strerror( error ),
	  interpret_type_at( decode_buffer, index ) ) ;

  LOG_DEBUG( "Read binary: '%s'.", res ) ;

  return res ;

}



/**
 * Reads the header of a list that is supposed to exist at current buffer
 * location, and sets the specified size accordingly (i.e. with the number of
 * elements in that list).
 *
 * Note: handles transparently the lists containing only small integers (0..255)
 * that are unintendendly interpreted as strings.
 *
 */
void read_list_header_parameter( input_buffer buffer, buffer_index * index,
								 arity * size )
{

 // Init:
  encoded_as_string = false ;

  if ( ei_decode_list_header( *buffer, index, size ) != 0 )
  {

	/* Could not be interpreted as a list, probably because term_to_binary/1
	 * mistook that list for a string...
	 *
	 */

	LOG_DEBUG( "List could not be successfully decoded as such; "
	  "actual detected type: %s (index is %i).",
	  interpret_type_at( buffer, index ), *index ) ;

	int type ;

	if ( ei_get_type( *buffer, index, &type, size ) != 0 )
	  raise_error( "The actual type of the list at %i could not be "
		"determined.", index ) ;

	if ( type != ERL_STRING_EXT )
	  raise_error( "The actual type of the list is not a string either, "
		"it is: %s.", interpret_type_at( buffer, index ) ) ;

	encoded_as_string = true ;

	if ( encoded_string != NULL )
	  free( encoded_string ) ;

	// Null-terminated:
	encoded_string = (char *) malloc( ( *size + 1 ) * sizeof( char ) ) ;

	if ( ei_decode_string( *buffer, index, encoded_string ) != 0 )
	  raise_error( "Detected an unintended string-encoding of parameters, "
		"yet was not able to decode it." ) ;

	LOG_DEBUG( "Switching to string-based list of size %i", *size ) ;

	encoded_index = 0 ;

	encoded_length = *size ;

	return ;

  }

  // Here we had a normal list, nothing more to do.
  LOG_DEBUG( "Normal list found at index %i, having %i element(s).",
	*index, *size ) ;

}




/* Second, setters of values, to encode from C to Erlang.
 *
 */




/**
 * Writes in specified return buffer the specified bool result.
 *
 */
void write_bool_result( output_buffer * output_sm_buf, bool b )
{

  if ( ei_x_encode_boolean( output_sm_buf, b ) != 0 )
	raise_error( "Erlang bool encoding failed." ) ;

}



/**
 * Writes in specified return buffer the specified (signed) integer result.
 *
 */
void write_int_result( output_buffer * output_sm_buf, int i )
{

  if ( ei_x_encode_long( output_sm_buf, (long) i ) != 0 )
	raise_error( "Erlang (signed) integer encoding failed." ) ;

}



/**
 * Writes in specified return buffer the specified unsigned integer result.
 *
 */
void write_unsigned_int_result( output_buffer * output_sm_buf, unsigned int u )
{

  if ( ei_x_encode_ulong( output_sm_buf, (unsigned long) u ) != 0 )
	raise_error( "Erlang unsigned integer encoding failed." ) ;

}



/**
 * Writes in specified return buffer the specified double result.
 *
 */
void write_double_result( output_buffer * output_sm_buf, double d )
{

  if ( ei_x_encode_double( output_sm_buf, d ) != 0 )
	raise_error( "Erlang double encoding failed." ) ;

}



/**
 * Writes in specified return buffer the specified atom result, based on
 * specified (NULL-terminated) string.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_atom_result( output_buffer * output_sm_buf, const char * atom_name )
{

  size_t len = strlen( atom_name ) ;

  if ( ei_x_encode_atom_len( output_sm_buf, atom_name, len ) != 0 )
	raise_error( "Erlang atom encoding failed for '%s'.", atom_name ) ;

  // Not owned: free( atom_name ) ;

}


/**
 * Writes in specified return buffer the specified (NULL-terminated) string
 * result, of specified length.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_string_with_length_result( output_buffer * output_sm_buf,
  const char * string, size_t length )
{

  // Not including the NULL terminator:
  if ( ei_x_encode_string_len( output_sm_buf, string, length ) != 0 )
	raise_error( "Erlang string encoding failed for '%s' (with length: %i).",
	  string, length ) ;

  // Not owned: free( string ) ;

}



/**
 * Writes in specified return buffer the specified (NULL-terminated) string
 * result.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_string_result( output_buffer * output_sm_buf, const char * string )
{

  size_t len = strlen( string ) ;

  // Not including the NULL terminator:
  if ( ei_x_encode_string_len( output_sm_buf, string, len ) != 0 )
	raise_error( "Erlang string encoding failed for '%s'.",
				 string ) ;

  // Not owned: free( string ) ;

}



/**
 * Writes in specified return buffer the specified binary result, of specified
 * size.
 *
 * Note: not taking ownership of the input binary.
 *
 */
void write_binary_result( output_buffer * output_sm_buf, const void * content,
						  byte_count size )
{

  if ( ei_x_encode_binary( output_sm_buf, content, size ) != 0 )
	raise_error( "Erlang binary encoding failed." ) ;

}



/**
 * Writes in specified return buffer the specified binary result obtained from a
 * string.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_binary_string_result( output_buffer * output_sm_buf,
  const char * string )
{

  // Not including the NULL terminator:
  if ( ei_x_encode_binary( output_sm_buf, string, strlen( string ) ) != 0 )
	raise_error( "Erlang binary encoding from string failed." ) ;

}



/**
 * Writes in specified return buffer the specified list header result.
 *
 * For a list declared of size N (hence having N elements), the N next writes
 * will correspond to the expected terms to form said list.
 *
 * See http://erlang.org/doc/man/ei.html#ei_x_encode_list_header to handle lists
 * whose size is not known a priori (see also write_empty_list_result/1).
 *
 */
void write_list_header_result( output_buffer * output_sm_buf, list_size size )
{

  if ( ei_x_encode_list_header( output_sm_buf, size ) != 0 )
	raise_error( "List header encoding failed (for size %i).", size ) ;

}



/**
 * Writes in specified return buffer an empty list.
 *
 * Especially useful to write lists whose size is not known a priori, by
 * cons'ing elements one by one until none is left.
 *
 * See http://erlang.org/doc/man/ei.html#ei_x_encode_list_header for more information.
 *
 */
void write_empty_list_result( output_buffer * output_sm_buf )
{

  if ( ei_x_encode_empty_list( output_sm_buf) != 0 )
	raise_error( "Empty list encoding failed" ) ;

}



/**
 * Writes in specified return buffer the specified tuple header result.
 *
 * For a tuple declared of size N (hence having N elements), the N next writes
 * will correspond to the expected terms to form said tuple.
 *
 */
void write_tuple_header_result( output_buffer * output_sm_buf, tuple_size size )
{

  if ( ei_x_encode_tuple_header( output_sm_buf, size ) != 0 )
	raise_error( "Tuple header encoding failed (for size %i).", size ) ;

}






// Lower-level primitives.


/**
 * Writes the specified number of bytes to the specified send buffer.
 *
 * Returns, if positive, the number of bytes written, otherwise an error code.
 *
  * (helper)
 *
 */
byte_count write_exact( byte * buf, byte_count len )
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
 * Sends the content of the specified smart buffer through the port's output
 * file descriptor.
 *
 * Returns the number of bytes written.
 *
 */
byte_count write_buffer( output_buffer * output_sm_buf )
{

  unsigned int len = output_sm_buf->index ;

  /* Not relevant anymore now that we use ei's smart buffer:

  if ( len + 2 > buffer_size )
  {

	raise_error( "Write length (%i) too high (buffer size: %i).",
	  len, input_buffer_size ) ;

	return -1 ;

  }

  */

  LOG_DEBUG( "Will write %i bytes.", len ) ;

  // Writes the 16-bit length, MSB-first:

  byte len_byte = ( len >> 8) & 0xff ;
  write_exact( &len_byte, 1 ) ;

  len_byte = len & 0xff ;
  write_exact( &len_byte, 1 ) ;

  return write_exact( output_sm_buf->buff, output_sm_buf->index ) ;

}




/**
 * Ensures that no invalid reading is done if being processing a
 * list-as-a-string.
 *
 */
void check_encoded_list()
{

 if ( encoded_as_string )
	raise_error( "Attempt to read a term whereas still in a string-based list "
	  "(at element %i/%i).", encoded_index, encoded_length ) ;

}



/**
 * Returns a string describing the type of the term located in specified buffer.
 *
 * Ownership of that string is transferred to the caller.
 *
 * This call will not impact the buffer or the index.
 *
 */
char * interpret_type_at( const input_buffer buffer,
  const buffer_index * index )
{

  int type ;
  int size ;

  ei_error error = ei_get_type( *buffer, index, &type, &size ) ;

  if ( error != 0 )
	raise_error(
	  "The type of the term at index %i could not be determined: %s.",
	  index, strerror( error ) ) ;

  char * res ;

  switch( type )
  {

  case ERL_SMALL_INTEGER_EXT:
	return strdup( "small integer" ) ;

  case ERL_INTEGER_EXT:
	return strdup( "integer" ) ;

  case ERL_FLOAT_EXT:
	return strdup( "float" ) ;

  case NEW_FLOAT_EXT:
	return strdup( "new float" ) ;

  case ERL_ATOM_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "atom of %i actual characters",
	  size );
	return res ;

  case ERL_SMALL_ATOM_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "small atom of %i actual characters",
	  size );
	return res ;

  case ERL_ATOM_UTF8_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "UTF8 atom of %i actual characters",
	  size );
	return res ;

  case ERL_SMALL_ATOM_UTF8_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "small UTF8 atom of %i actual characters",
	  size );
	return res ;

  case ERL_REFERENCE_EXT:
	return strdup( "reference" ) ;

  case ERL_NEW_REFERENCE_EXT:
	return strdup( "new reference" ) ;

  case ERL_NEWER_REFERENCE_EXT:
	return strdup( "newer reference" ) ;

  case ERL_PORT_EXT:
	return strdup( "port" ) ;

  case ERL_NEW_PORT_EXT:
	return strdup( "new port" ) ;

  case ERL_PID_EXT:
	return strdup( "PID" ) ;

  case ERL_NEW_PID_EXT:
	return strdup( "new PID" ) ;

  case ERL_SMALL_TUPLE_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "small tuple of %i elements",
	  size );
	return res ;

  case ERL_LARGE_TUPLE_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "large tuple of %i elements",
	  size );
	return res ;

  case ERL_NIL_EXT:
	return strdup( "empty list" ) ;

  case ERL_STRING_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "string of %i actual characters",
	  size );
	return res ;

  case ERL_LIST_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "list of %i elements",
	  size );
	return res ;

  case ERL_BINARY_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "binary of %i bytes",
	  size );
	return res ;

  case ERL_BIT_BINARY_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "bit binary of %i bytes",
	  size );
	return res ;

  case ERL_SMALL_BIG_EXT:
	return strdup( "small big integer" ) ;

  case ERL_LARGE_BIG_EXT:
	return strdup( "large big integer" ) ;

  case ERL_NEW_FUN_EXT:
	return strdup( "new fun" ) ;

  case ERL_MAP_EXT:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "map of %i entries",
	  size );
	return res ;

  case ERL_FUN_EXT:
	return strdup( "fun" ) ;

  case ERL_EXPORT_EXT:
	return strdup( "export" ) ;

  case ERL_NEW_CACHE:
	return strdup( "new cache" ) ;

  case ERL_CACHED_ATOM:
	res = (char*) malloc( string_size * sizeof( char ) ) ;
	snprintf( res, string_size, "cached atom of %i actual characters",
	  size );
	return res ;

  default:
	return strdup( "unexpected type (abnormal)" ) ;

  }

}
