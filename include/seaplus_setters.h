/*
 * Copyright (C) 2018-2025 Olivier Boudeville
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
 * Writes in specified return buffer the specified boolean result.
 *
 */
void write_bool_result( output_buffer * output_sm_buf, bool b ) ;


/**
 * Writes in specified return buffer the specified (signed) integer result.
 *
 */
void write_int_result( output_buffer * output_sm_buf, int i ) ;


/**
 * Writes in specified return buffer the specified unsigned integer result.
 *
 */
void write_unsigned_int_result( output_buffer * output_sm_buf, unsigned int u ) ;


/**
 * Writes in specified return buffer the specified double result.
 *
 */
void write_double_result( output_buffer * output_sm_buf, double d ) ;


/**
 * Writes in specified return buffer the specified atom result, based on
 * specified (NULL-terminated) string.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_atom_result( output_buffer * output_sm_buf, const char * atom_name ) ;


/**
 * Writes in specified return buffer the specified (NULL-terminated) string
 * result, of specified length.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_string_with_length_result( output_buffer * output_sm_buf, const char * string,
  size_t length ) ;


/**
 * Writes in specified return buffer the specified string result.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_string_result( output_buffer * output_sm_buf, const char * string ) ;


/**
 * Writes in specified return buffer the specified binary result, of specified
 * size.
 *
 * Note: not taking ownership of the input binary.
 *
 */
void write_binary_result( output_buffer * output_sm_buf, const void * content,
  byte_count size ) ;


/**
 * Writes in specified return buffer the specified binary result obtained from a
 * string.
 *
 * Note: not taking ownership of the input string.
 *
 */
void write_binary_string_result( output_buffer * output_sm_buf,
  const char * string ) ;



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
void write_list_header_result( output_buffer * output_sm_buf,
  list_size size ) ;



/**
 * Writes in specified return buffer an empty list.
 *
 * Especially useful to write lists whose size is not known a priori, by
 * cons'ing elements one by one until none is left.
 *
 * See http://erlang.org/doc/man/ei.html#ei_x_encode_list_header for more information.
 *
 */
void write_empty_list_result( output_buffer * output_sm_buf ) ;



/**
 * Writes in specified return buffer the specified tuple header result.
 *
 * For a tuple declared of size N (hence having N elements), the N next writes
 * will correspond to the expected terms to form said tuple.
 *
 */
void write_tuple_header_result( output_buffer * output_sm_buf,
  tuple_size size ) ;


/**
 * Sends the content of the specified buffer through the port's output file
 * descriptor.
 *
 * Returns the number of bytes written.
 *
 */
byte_count write_buffer( output_buffer * output_sm_buf ) ;


#endif // _SEAPLUS_SETTERS_H_
