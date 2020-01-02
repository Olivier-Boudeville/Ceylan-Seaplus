/*
 * Copyright (C) 2018-2020 Olivier Boudeville
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

#ifndef _SEAPLUS_GETTERS_H_
#define _SEAPLUS_GETTERS_H_


/**
 * Determines, from specified buffer, the information regarding the
 * (Erlang-side) specified function, namely its function identifier and its
 * parameters, set in the variables whose reference is specified.
 *
 */
void read_function_information( input_buffer buffer, buffer_index * index,
  fun_id * current_fun_id, arity * param_count ) ;



// Accessors to (parameter) values (getters).


/**
 * Returns the element at current buffer location, supposed to be a (long,
 * signed) integer.
 *
 */
long read_int_parameter( input_buffer decode_buffer, buffer_index * index ) ;



/**
 * Returns the element at current buffer location, supposed to be a (long,
 * unsigned) integer.
 *
 */
unsigned long read_unsigned_int_parameter( input_buffer decode_buffer,
  buffer_index * index ) ;



/**
 * Returns the element at current buffer location, supposed to be a double.
 *
 */
double read_double_parameter( input_buffer decode_buffer, buffer_index * index ) ;



/**
 * Returns the element at current buffer location, supposed to be an atom,
 * translated to a char*, whose ownership is transferred to the caller (who is
 * thus supposed to deallocate it ultimately, with standard free/1).
 *
 */
char * read_atom_parameter( input_buffer decode_buffer, buffer_index * index ) ;



/**
 * Returns the element at current buffer location, supposed to be a (plain)
 * string, whose ownership is transferred to the caller (who is thus supposed to
 * deallocate it ultimately, with standard free/1).
 *
 */
char * read_string_parameter( input_buffer decode_buffer, buffer_index * index ) ;



/**
 * Returns the element at current buffer location, supposed to be a binary,
 * returned as a string whose ownership is transferred to the caller (who is
 * thus supposed to deallocate it ultimately, with standard free/1).
 *
 */
char * read_binary_parameter( input_buffer decode_buffer, buffer_index * index ) ;


/**
 * Reads the header of a list that is supposed to exist at current buffer
 * location, and sets the specified size accordingly (i.e. with the number of
 * elements in that list).
 *
 * Note: handles transparently the lists containing only small integers (0..255)
 * that are unintendendly interpreted as strings.
 *
 */
void read_list_header_parameter( input_buffer decode_buffer, buffer_index * index,
								 arity * size ) ;


#endif // _SEAPLUS_GETTERS_H_
