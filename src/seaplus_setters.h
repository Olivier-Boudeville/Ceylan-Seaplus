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

#ifndef _SEAPLUS_SETTERS_H_
#define _SEAPLUS_SETTERS_H_



// Accessors to (result) values (setters):


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
 * Writes in specified return buffer the specified double result.
 *
 */
void write_as_double( byte * buffer, double d ) ;


/**
 * Writes in specified return buffer the specified string result.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_as_string( byte * buffer, const char * string ) ;


/**
 * Writes in specified return buffer the specified binary result.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_as_binary( byte * buffer, const char * string ) ;



/**
 * Sends the content of the specified buffer through the port's output file
 * descriptor.
 *
 * Returns the number of bytes written.
 *
 */
byte_count write_buffer( byte *buf, byte_count len ) ;


#endif // _SEAPLUS_SETTERS_H_
