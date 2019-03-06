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

#ifndef _SEAPLUS_H_
#define _SEAPLUS_H_


// Seaplus-defined types:

// To address the message buffer:
typedef unsigned char byte;


// As a number of bytes (negative values meaning errors):
typedef int byte_count ;


// Designates an index in a buffer:
typedef int buffer_index ;

// Designates the index of an element in a tuple (start at 1):
typedef unsigned int tuple_index ;


// Size of the buffer for port input/output:
extern const byte_count buffer_size ;


// Seaplus reference onto an Erlang API function:
typedef unsigned int fun_id ;

// The arity of an Erlang function:
typedef unsigned int arity ;

typedef unsigned int list_size ;

typedef unsigned int string_len ;

typedef unsigned int tuple_size ;


// For bool:
#include <stdbool.h>


// Seaplus-provided functions (roughly listed in their expected order of use)


// No forward declaration seems possible:
//struct ETERM ;

// So:

// For ETERM and all:
#include "erl_interface.h"


// By default, enable logging:
#ifndef SEAPLUS_ENABLE_LOG

#define SEAPLUS_ENABLE_LOG 1

#endif // SEAPLUS_ENABLE_LOG


/**
 * Starts the C driver.
 *
 * Returns the encoding/decoding buffer.
 *
 */
byte * start_seaplus_driver() ;



#if SEAPLUS_ENABLE_LOG

#define LOG_DEBUG( format, ... ) log_debug( format, ##__VA_ARGS__ )
#define LOG_TRACE( format, ... ) log_trace( format, ##__VA_ARGS__ )

#else // SEAPLUS_ENABLE_LOG

#define LOG_DEBUG( format, ... )
#define LOG_TRACE( format, ... )

#endif // SEAPLUS_ENABLE_LOG



// Logs specified debug message.
void log_debug( const char * format, ... ) ;

// Logs specified trace message.
void log_trace( const char * format, ... ) ;


// Raises specified error: reports it in logs, and halts.
void raise_error( const char * format, ... ) ;


/**
 * Receives the next command from the port's input file descriptor, and stores
 * it in the specified buffer.
 *
 */
byte_count read_command( byte *buf ) ;


/**
 * Raises an error should the actual arity not be the expected one.
 *
 */
void check_arity_is( arity expected, arity actual, fun_id id ) ;




// First, accessors to values (getters).



/// For tuples:


/**
 * Returns the element i of specified tuple, as a term that shall be
 * deallocated.
 *
 */
ETERM * get_element_from_tuple( tuple_index i, ETERM *tuple_term ) ;


// Returns the element i of specified tuple, as an integer.
int get_element_as_int( tuple_index i, ETERM *tuple_term ) ;


/**
 * Returns the element i of specified tuple, as an unsigned integer.
 *
 * Note: apparently, Erlang integers are rather returned as 'int', not 'unsigned
 * int'.
 *
 */
unsigned int get_element_as_unsigned_int( tuple_index i, ETERM *tuple_term ) ;

// Returns the element i of specified tuple, as a double.
double get_element_as_double( tuple_index i, ETERM *tuple_term ) ;


/**
 * Returns the element i of specified tuple, as a string (char *).
 *
 * Ownership of the returned string transferred to the caller.
 *
 * Note: cannot return a const char*, as the caller is to deallocate that
 * string.
 *
 */
char * get_element_as_string( tuple_index i, ETERM *tuple_term ) ;



/// For lists:


/**
 * Returns the head of the specified, supposedly non-empty, list.
 *
 * Note that the return term shall be freed (with erl_free_term/1) by the
 * caller.
 *
 */
ETERM * get_head( ETERM * list_term ) ;


/**
 * Returns the tail of the specified, supposedly non-empty, list.
 *
 * Note that the return term shall be freed (with erl_free_term/1) by the
 * caller.
 *
 */
ETERM * get_tail( ETERM * list_term ) ;


/**
 * Returns the head of the specified, supposedly non-empty, list, as an integer.
 *
 */
int get_head_as_int( ETERM * list_term ) ;


/**
 * Returns the head of the specified, supposedly non-empty, list, as an unsigned
 * integer.
 *
 */
unsigned int get_head_as_unsigned_int( ETERM * list_term ) ;


/**
 * Returns the head of the specified, supposedly non-empty, list, as a double.
 *
 */
double get_head_as_double( ETERM * list_term ) ;


/**
 * Returns the head of the specified, supposedly non-empty, list as an atom
 * (translated to a char*)
 *
 * Ownership of the returned string transferred to the caller (who shall use
 * erl_free/1 to deallocate it).
 *
 */
char * get_head_as_atom( ETERM * list_term ) ;


/**
 * Returns the head of the specified, supposedly non-empty, list, as a string.
 *
 * Ownership of the returned string transferred to the caller (who shall use
 * erl_free/1 to deallocate it).
 *
 */
char * get_head_as_string( ETERM * list_term ) ;





// Second, setters of values:


/**
 * Writes specified term into specified buffer.
 *
 * Takes ownership, and deallocates, specified term.
 *
 */
void write_term( byte * buffer, ETERM * term ) ;


/**
 * Writes in specified return buffer the specified boolean result.
 *
 */
void write_as_bool( byte * buffer, bool b ) ;


/**
 * Writes in specified return buffer the specified (signed) integer result.
 *
 */
void write_as_int( byte * buffer, int i ) ;


/**
 * Writes in specified return buffer the specified unsigned integer result.
 *
 */
void write_as_unsigned_int( byte * buffer, unsigned int u ) ;


/**
 * Writes in specified return buffer the specified string result.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_as_string( byte * buffer, char * string ) ;


/**
 * Writes in specified return buffer the specified binary result.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_as_binary( byte * buffer, char * string ) ;



/**
 * Sends the content of the specified buffer through the port's output file
 * descriptor.
 *
 * Returns the number of bytes written.
 *
 */
byte_count write_buffer( byte *buf, byte_count len ) ;



/**
 * Stops the C driver.
 *
 */
void stop_seaplus_driver( byte * buffer ) ;


// Mostly exported for separate testing:

void start_logging( const char * log_filename ) ;

void stop_logging() ;


#endif // _SEAPLUS_H_
