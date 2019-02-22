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


/* erl_comm.c */

#include "seaplus.h"


// For read and write:
#include <unistd.h>

// For strlen:
#include <string.h>

// For free:
#include <stdlib.h>





/**
 * Starts the C driver.
 *
 * Returns the encoding/decoding buffer.
 *
 */
byte * start_seaplus_driver()
{

  LOG_DEBUG( "Starting the Seaplus C driver." ) ;

  /* Initiating memory handling, always as:
	 (see http://erlang.org/doc/man/erl_eterm.html#erl_init)
   */
  erl_init( NULL, 0 ) ;

  byte * buffer = (byte *) malloc( buffer_size ) ;

  return buffer ;

}


/**
 * Stops the C driver.
 *
 */
void stop_seaplus_driver( byte * buffer )
{

  LOG_DEBUG( "Stopping the Seaplus C driver." ) ;

  free( buffer ) ;

}



/**
 * Receives the next command from the port's input file descriptor, and stores
 * it in the specified buffer.
 *
 */
int read_seaplus_command( byte *buf )
{

  int len ;

  // Two bytes for command length:
  if ( read_exact( buf, 2 ) != 2 )
  {

	LOG_ERROR( "Reading of the length of the command buffer failed." ) ;
	return (-1) ;

  }

  len = (buf[0] << 8) | buf[1] ;

  LOG_DEBUG( "Will read %i bytes.", len ) ;

  if ( len + 2 > buffer_size )
  {

	LOG_ERROR( "Read length (%i) too high (buffer size: %i).",
	  len, buffer_size ) ;
	return (-1) ;

  }

  return read_exact( buf, len ) ;

}



/**
 * Reads the specified number of bytes from the specified receive buffer.
 *
 * Returns, if positive, the number of bytes read, otherwise an error code.
 *
 * (helper)
 *
 */
message_size read_exact( byte *buf, message_size len )
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
 * Sends the command result stored in the specified buffer to the port's output
 * file descriptor.
 *
 */
message_size write_seaplus_command( byte *buf, message_size len )
{

  LOG_DEBUG( "Will write %i bytes.", len ) ;

  if ( len + 2 > message_size )
  {

	LOG_ERROR( "Write length (%i) too high (buffer size: %i).",
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



/**
 * Writes the specified number of bytes to the specified send buffer.
 *
 * Returns, if positive, the number of bytes written, otherwise an error code.
 *
 * (helper)
 *
 */
int write_exact( byte *buf, message_size len )
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




/* First, accessors to values (getters) returned by erl_decode.
 *
 * Note: visibly, ownership of the result of that call is transferred to the
 * caller, who has thus to deallocate them, which is done in these getters.
 *
 */




// Returns the element i of specified tuple, as an int.
signed int get_as_int( tuple_index i, ETERM *tupleTerm )
{

  ETERM *elem = erl_element( i, tupleTerm ) ;

  int res = ERL_INT_VALUE( elem ) ;

  erl_free_term( elem ) ;

  return res ;

}


// Returns the element i of specified tuple, as an unsigned int.
unsigned int get_as_unsigned_int( tuple_index i, ETERM *tupleTerm )
{

  ETERM *elem = erl_element( i, tupleTerm ) ;

  unsigned int res = ERL_INT_UVALUE( elem ) ;

  erl_free_term( elem ) ;

  return res ;

}


// Returns the element i of specified tuple, as a double.
double get_as_double( tuple_index i, ETERM *tupleTerm )
{

  ETERM *elem = erl_element( i, tupleTerm ) ;

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
char * get_as_string( tuple_index i, ETERM *tupleTerm )
{

  ETERM *elem = erl_element( i, tupleTerm ) ;

  // Expected to be null-terminated:
  char * stringContent = (char *) ERL_BIN_PTR( elem ) ;

  int size = ERL_BIN_SIZE( elem ) ;

  // Bug somewhere...

  char * res = (char *) malloc( size * sizeof(char) ) ;

  strncpy( res, stringContent, size ) ;

  erl_free_term( elem ) ;

  return res ;

}




/* Second, setters of values accepted by erl_encode.
 *
 */


/**
 * Writes in specified return buffer the specified (signed) integer result.
 *
 */
void write_as_int( byte * buffer, int result )
{

  // Constructing the ETERM struct that represents the integer result:
  ETERM * intTerm = erl_mk_int( result ) ;

  // Encodes that ETERM into the (send) buffer:
  erl_encode( intTerm, buffer ) ;

  // Sends that buffer:
  write_seaplus_command( buffer, erl_term_len( intTerm ) ) ;

  // Clean-up ETERM memory allocated for this round:
  erl_free_term( intTerm ) ;

}


/**
 * Writes in specified return buffer the specified unsigned integer result.
 *
 */
void write_as_unsigned_int( byte * buffer, unsigned int result )
{

  // Constructing the ETERM struct that represents the unsigned integer result:
  ETERM * uintTerm = erl_mk_uint( result ) ;

  // Encodes that ETERM into the (send) buffer:
  erl_encode( uintTerm, buffer ) ;

  // Sends that buffer:
  write_seaplus_command( buffer, erl_term_len( uintTerm ) ) ;

  // Clean-up ETERM memory allocated for this round:
  erl_free_term( uintTerm ) ;

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

  ETERM * stringTerm = erl_mk_estring( string, len ) ;

  // Encodes that ETERM into the (send) buffer:
  erl_encode( stringTerm, buffer ) ;

  // Sends that buffer:
  write_seaplus_command( buffer, erl_term_len( stringTerm ) ) ;

  // Clean-up ETERM memory allocated for this round:
  erl_free_term( stringTerm ) ;

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

  ETERM * binaryTerm = erl_mk_binary( string, len ) ;

  // Encodes that ETERM into the (send) buffer:
  erl_encode( binaryTerm, buffer ) ;

  // Sends that buffer:
  write_seaplus_command( buffer, erl_term_len( binaryTerm ) ) ;

  // Clean-up ETERM memory allocated for this round:
  erl_free_term( binaryTerm ) ;

  // Not owned: free( string ) ;

}
