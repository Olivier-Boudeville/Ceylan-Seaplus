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

#ifndef _SEAPLUS_GETTERS_H_
#define _SEAPLUS_GETTERS_H_


/**
 * Determines, from specified buffer, the information regarding the
 * (Erlang-side) specified function, namely its function identifier and its
 * parameters, set in the variables whose reference is specified.
 *
 * Returns a term that is to be deallocated once all parameters will have been
 * used.
 *
 */
ETERM * get_function_information( byte * buffer, fun_id * current_fun_id,
  arity * param_count, ETERM *** parameters ) ;


// Accessors to (parameter) values (getters).



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


// char * get_element_as_atom( tuple_index i, ETERM *tuple_term ) is lacking.


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
 * Returns the head of the specified, supposedly non-empty, list, as a C double.
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



// Parameter-based getters:



/**
 * Returns the element at specified index of specified array of parameters,
 * supposed to be an integer.
 *
 * Note: the corresponding terms is not freed, as the parameter array is
 * expected to be deallocated as a whole (recursively) afterwards.
 *
 */
int get_parameter_as_int( parameter_index index, ETERM ** parameters ) ;


/**
 * Returns the element at specified index of specified array of parameters,
 * supposed to be an unsigned integer.
 *
 * Note: the corresponding terms is not freed, as the parameter array is
 * expected to be deallocated as a whole (recursively) afterwards.
 *
 */
unsigned int get_parameter_as_unsigned_int( parameter_index index,
  ETERM ** parameters ) ;


/**
 * Returns the element at specified index of specified array of parameters,
 * supposed to be an Erlang float (hence returning a C double).
 *
 * Note: the corresponding terms is not freed, as the parameter array is
 * expected to be deallocated as a whole (recursively) afterwards.
 *
 */
double get_parameter_as_double( parameter_index index, ETERM ** parameters ) ;


/**
 * Returns the element at specified index of specified array of parameters,
 * supposed to be an atom, translated to a char*.
 *
 * Note: the corresponding terms is not freed, as the parameter array is
 * expected to be deallocated as a whole (recursively) afterwards.
 *
 * Ownership of the returned string transferred to the caller (who shall use
 * erl_free/1 to deallocate it).
 *
 */
char * get_parameter_as_atom( parameter_index index, ETERM ** parameters ) ;


/**
 * Returns the element at specified index of specified array of parameters,
 * supposed to be a string.
 *
 * Note: the corresponding terms is not freed, as the parameter array is
 * expected to be deallocated as a whole (recursively) afterwards.
 *
 * Ownership of the returned string transferred to the caller (who shall use
 * erl_free/1 to deallocate it).
 *
 */
char * get_parameter_as_string( parameter_index index, ETERM ** parameters ) ;



#endif // _SEAPLUS_GETTERS_H_
