/* erl_comm.c */

#include "erlang_binding_helpers.h"


// For read and write:
#include <unistd.h>

// For strlen:
#include <string.h>

// For free:
# include <stdlib.h>


/*
 * Starts the C driver.
 *
 * Returns the encoding/decoding buffer.
 *
 */
byte * start_driver()
{

   LOG_DEBUG( "Starting C driver." ) ;

  /* Initiating memory handling, always as:
	 (see http://erlang.org/doc/man/erl_eterm.html#erl_init)
   */
  erl_init( NULL, 0 ) ;

  byte * buffer = (byte *) malloc( 1024 ) ;

  return buffer ;

}


/*
 * Stops the C driver.
 *
 */
void stop_driver( byte * buffer )
{

  LOG_DEBUG( "Stopping C driver." ) ;

  free( buffer ) ;

}



/**
 * Reads next command from the specified buffer.
 *
 */
int read_command( byte *buf )
{

  int len ;

  if ( read_exact( buf, 2 ) != 2 )
  {

	LOG_ERROR( "Reading of the length of the command buffer failed." ) ;
	return (-1) ;

  }

  len = (buf[0] << 8) | buf[1] ;

  LOG_DEBUG( "Read %i bytes.", len ) ;

  return read_exact( buf, len ) ;

}



/**
 * Reads specified number of bytes from the specified buffer.
 *
 * Returns, if positive, the number of bytes read, otherwise an error code.
 *
 */
int read_exact( byte *buf, int len )
{

  int i, got=0 ;

  do
  {

	if ( (i = read(0, buf+got, len-got) ) <= 0 )
	  return (i) ;

	got += i ;

  } while ( got < len ) ;

  return( len ) ;

}



int write_command( byte *buf, int len )
{

  byte li;

  li = (len >> 8) & 0xff ;
  write_exact( &li, 1 ) ;

  li = len & 0xff ;
  write_exact( &li, 1 ) ;

  return write_exact( buf, len ) ;

}


int write_exact( byte *buf, int len )
{

  int i, wrote = 0 ;

  do
  {

	if ( (i = write( 1, buf+wrote, len-wrote ) ) <= 0 )
	  return (i) ;

	wrote += i ;

  } while ( wrote < len ) ;

  return ( len ) ;

}


/* Accessors to values returned by erl_decode.
 *
 * Note: visibly, ownership of the result of that call is transfered to the
 * caller, who has thus to deallocate them, which is done in these getters?.
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

  int res = ERL_INT_UVALUE( elem ) ;

  erl_free_term( elem ) ;

  return res ;

}


// Returns the element i of specified tuple, as a float.
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

  // Bogue quelque part...

  char * res = (char *) malloc( size * sizeof(char) ) ;

  strncpy( res, stringContent, size ) ;

  erl_free_term( elem ) ;

  return res ;

}


/*
 *  Writes in specified return buffer the specified (signed) integer result.
 *
 */
void write_as_int( byte * buffer, ETERM * paramTuple, int result )
{

  // Constructing the ETERM struct that represents the integer result:
  ETERM * intTerm = erl_mk_int( result ) ;

  // Encodes that ETERM into the (send) buffer:
  erl_encode( intTerm, buffer ) ;

  // Sends that buffer:
  write_command( buffer, erl_term_len( intTerm ) ) ;

  // Clean-up ETERM memory allocated for this round:
  erl_free_compound( paramTuple ) ;
  erl_free_term( intTerm ) ;

}


/*
 *  Writes in specified return buffer the specified integer result.
 *
 */
void write_as_unsigned_int( byte * buffer, ETERM * paramTuple,
							unsigned int result )
{

  // Constructing the ETERM struct that represents the unsigned integer result:
  ETERM * uintTerm = erl_mk_uint( result ) ;

  // Encodes that ETERM into the (send) buffer:
  erl_encode( uintTerm, buffer ) ;

  // Sends that buffer:
  write_command( buffer, erl_term_len( uintTerm ) ) ;

  // Clean-up ETERM memory allocated for this round:
  erl_free_compound( paramTuple ) ;
  erl_free_term( uintTerm ) ;

}


/*
 *  Writes in specified return buffer the specified string result.
 *
 * Note: takes ownership of the input string.
 *
 */
void write_as_string( byte * buffer, ETERM * paramTuple, char * string )
{

  // Constructing the ETERM struct that represents the string result:

  size_t len = strlen( string ) ;

  ETERM * stringTerm = erl_mk_estring( string, len ) ;

  // Encodes that ETERM into the (send) buffer:
  erl_encode( stringTerm, buffer ) ;

  // Sends that buffer:
  write_command( buffer, erl_term_len( stringTerm ) ) ;

  // Clean-up ETERM memory allocated for this round:
  erl_free_compound( paramTuple ) ;
  erl_free_term( stringTerm ) ;
  free( string ) ;

}


/*
 *  Writes in specified return buffer the specified binary result.
 *
 * Note: takes ownership of the input string.
 *
 */
void write_as_binary( byte * buffer, ETERM * paramTuple, char * string )
{

  // Constructing the ETERM struct that represents the string result:

  size_t len = strlen( string ) ;

  ETERM * binaryTerm = erl_mk_binary( string, len ) ;

  // Encodes that ETERM into the (send) buffer:
  erl_encode( binaryTerm, buffer ) ;

  // Sends that buffer:
  write_command( buffer, erl_term_len( binaryTerm ) ) ;

  // Clean-up ETERM memory allocated for this round:
  erl_free_compound( paramTuple ) ;
  erl_free_term( binaryTerm ) ;
  free( string ) ;

}
