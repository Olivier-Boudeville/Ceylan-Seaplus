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


// To address the message buffer:
typedef unsigned char byte;


// As a number of bytes (negative values meaning errors):
typedef int message_size ;


// Designates the index of an element in a tuple:
typedef unsigned int tuple_index ;


// Size of the buffer for port input/output:
const message_size buffer_size = 4096*8 ;


// Seaplus reference onto an Erlang API function:
typedef unsigned int fun_id ;


// Forward declarations:

message_size read_seaplus_command( byte *buf ) ;
message_size write_seaplus_command( byte *buf, message_size len ) ;



// No forward declaration seems possible:
//struct ETERM ;

// So:

// For ETERM and all:
#include "erl_interface.h"

byte * start_seaplus_driver() ;
void stop_seaplus_driver( byte * buffer ) ;

int get_as_int( tuple_index i, ETERM *tupleTerm ) ;
unsigned int get_as_unsigned_int( tuple_index i, ETERM *tupleTerm ) ;
double get_as_double( tuple_index i, ETERM *tupleTerm ) ;
char * get_as_string( tuple_index i, ETERM *tupleTerm ) ;

void write_as_int( byte * buffer, ETERM * paramTuple, int result ) ;

void write_as_unsigned_int( byte * buffer, ETERM * paramTuple,
							unsigned int result ) ;

void write_as_string( byte * buffer, ETERM * paramTuple, char * string ) ;
void write_as_binary( byte * buffer, ETERM * paramTuple, char * string ) ;


/*
 * Logs are displayed as expected on the console.
 *
 * Directly inspired from
 * http://www.valvers.com/programming/c/logging-with-gcc/:
 *
 */

#ifdef DEBUG_SEAPLUS


#include <stdio.h>

#define LOG_DEBUG(x, ...) do { fprintf( stderr, "[%s:%s:%d] " x "\r\n", "DBG", __func__, __LINE__, ##__VA_ARGS__) ; } while (0)


#else // DEBUG_SEAPLUS


#define LOG_DEBUG(x, ...) do { } while(0)

#endif // DEBUG_SEAPLUS


#define LOG_ERROR(x, ...) do { fprintf( stderr, "[%s:%s:%d] " x "\r\n", "ERR", __func__, __LINE__, ##__VA_ARGS__) ; } while (0)


#endif // _SEAPLUS_H_
