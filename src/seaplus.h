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


// For ssize_t:
#include <unistd.h>


// For ei_x_buff and encoding/decoding functions:
#include "ei.h"


// Seaplus-defined types:


/**
 * To address the message buffer:
 * (signed, to accommodate ei):
 *
 */
//typedef unsigned char byte;
typedef char byte;


// A smart buffer for encoding:
typedef ei_x_buff smart_buffer ;


// As a number of bytes (negative values meaning errors):
typedef ssize_t byte_count ;


// Designates an index in a buffer:
typedef int buffer_index ;

// Designates the index of an element in a tuple (starts at 1):
typedef unsigned int tuple_index ;


// Size of the buffer for port input/output:
extern const byte_count buffer_size ;


// Seaplus reference onto an Erlang API function:
typedef long int fun_id ;

// The arity of an Erlang function (i.e. a count of parameters):
typedef int arity ;

// Index of a parameter (starts at 1):
typedef unsigned int parameter_index ;


typedef unsigned int list_size ;

typedef unsigned int string_len ;

// To accommodate ei:
//typedef unsigned int tuple_size ;
typedef int tuple_size ;

typedef int ei_error ;

// For bool:
#include <stdbool.h>


// Seaplus-provided functions (roughly listed in their expected order of use)



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
#define LOG_WARNING( format, ... ) log_warning( format, ##__VA_ARGS__ )

#else // SEAPLUS_ENABLE_LOG

#define LOG_DEBUG( format, ... )
#define LOG_TRACE( format, ... )
#define LOG_WARNING( format, ... )

#endif // SEAPLUS_ENABLE_LOG



// Logs specified debug message.
void log_debug( const char * format, ... ) ;

// Logs specified trace message.
void log_trace( const char * format, ... ) ;

// Logs specified warning message.
void log_warning( const char * format, ... ) ;


// Raises specified error: reports it in logs, and halts.
void raise_error( const char * format, ... ) ;


/// Prepares for the encoding of the next upcoming command.
void prepare_for_command( smart_buffer * sm_buf ) ;


/// Finalizes the current command.
void finalize_command( smart_buffer * sm_buf ) ;


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



// First, accessors to (parameter) values (getters).
#include "seaplus_getters.h"


// Second, accessors to (result) values (setters):
#include "seaplus_setters.h"



/**
 * Stops the C driver.
 *
 */
void stop_seaplus_driver( byte * buffer ) ;


// Mostly exported for separate testing:

void start_logging( const char * log_filename ) ;

void stop_logging() ;


#endif // _SEAPLUS_H_
