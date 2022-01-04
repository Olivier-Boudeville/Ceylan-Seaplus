/*
 * Copyright (C) 2018-2022 Olivier Boudeville
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
 * Creation date: Sunday, December 16, 2018.
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
 *
 * (switched to signed, to accommodate ei; beware to size computation when
 * performing bitwise operations)
 *
 */
//typedef unsigned char byte;
typedef char byte;


/* Opaque type.
 *
 * Means that the input_buffer is a pointer to an array of bytes.
 *
 * The extra pointer indirection is necessary so that the array may be
 * realloc'ed, should a large enough term have to be read.
 *
 */
typedef byte **input_buffer ;


// A smart buffer for encoding (meant to be an opaque type):
typedef ei_x_buff output_buffer ;


// As a number of bytes (negative values meaning errors):
typedef ssize_t byte_count ;


// Designates an index in a buffer:
typedef int buffer_index ;

// Designates the index of an element in a tuple (starts at 1):
typedef unsigned int tuple_index ;


// Size of the buffer for input/output:
extern byte_count input_buffer_size ;


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
 * Sets the input buffer.
 *
 */
void start_seaplus_driver( input_buffer buf ) ;



#if SEAPLUS_ENABLE_LOG

/* Note that ## is rather gcc-specific:
 * (https://gcc.gnu.org/onlinedocs/gcc/Variadic-Macros.html)
 *
 */

#define LOG_DEBUG( format, ... ) log_debug( format, ## __VA_ARGS__ )
#define LOG_TRACE( format, ... ) log_trace( format, ## __VA_ARGS__ )
#define LOG_WARNING( format, ... ) log_warning( format, ## __VA_ARGS__ )

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



/* Raises specified error, based on a specified arguments: reports it in logs,
 * and halts.
 *
 */
void raise_error( const char * format, ... ) ;


/* Raises specified error, based on a variadic argument: reports it in logs, and
 * halts.
 *
 * Useful to report errors from a driver. For an example, see in Ceylan-Mobile:
 *   void raise_gammu_error( GSM_StateMachine * gammu_fsm,
 *                           const char * format, ... )
 *
 */
void raise_error_variadic( const char * format, va_list *values ) ;


/**
 * Prepares for the encoding of the next upcoming command.
 *
 * A buffer is specified, as in some cases (ex: interrupt handling) multiple
 * output buffers may be useful.
 *
 */
void prepare_for_command( output_buffer * output_sm_buf ) ;


/**
 * Finalizes the current command, provided it performed directly at least one
 * write, which is by far the most general case.
 *
 * Not to be called if the actual writing is to be done elsewhere (ex: from an
 * interrupt handler triggered later, asynchronously).
 *
 */
void finalize_command_after_writing( output_buffer * output_sm_buf ) ;


/**
 * Receives the next command from the port's input file descriptor, and stores
 * it in the specified buffer.
 *
 */
byte_count read_command( input_buffer buf ) ;


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
 * Initializes the main smart buffer (the internal fields thereof) according to
 * the Erlang binary format.
 *
 */
void init_output_buffer( output_buffer * sm_buf ) ;


/**
 * Clears the specified (main or auxiliary) smart buffer (the internal fields
 * thereof).
 *
 */
void clear_output_buffer( output_buffer * sm_buf ) ;



/**
 * Stops the C driver.
 *
 */
void stop_seaplus_driver( input_buffer buffer ) ;


// Mostly exported for separate testing:

void start_logging( const char * log_filename ) ;

void stop_logging() ;


#endif // _SEAPLUS_H_
