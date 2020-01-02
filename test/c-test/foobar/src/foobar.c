/*
 * Copyright (C) 2018-2020 Olivier Boudeville
 *
 * This file is part of the Ceylan-Seaplus tests and examples.
 *
 * It has been placed in the public domain.
 *
 * Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
 *
 */

/* Example of code to be gathered in a library typically called by a C-wrapper
 * available as an Erlang module.
 *
 * Having access to this source code is not necessary at all to bridge this
 * service with Erlang: this is not an element that Seaplus or the service
 * integrator control.
 *
 */


// For malloc:
#include <stdlib.h>

// For sprintf:
#include <stdio.h>

// For strlen:
#include <string.h>



// For service function and type declarations:
#include "foobar.h"



int foo( int a )
{

  if ( a != 0 )
  {
	return a + 1 ;
  }
  else
  {

	// A good old division by zero is perfect to test C-side failures:

	int b = 1 ;

	return b / a ;

  }

}



/* Ownership of returned struct is transferred to the caller, which is thus
 * expected to deallocate it at some time in the future.
 *
 */
struct foo_data * bar( double a, enum foo_status status )
{

  struct foo_data * res = malloc( sizeof( struct foo_data ) ) ;

  if ( status == moderate_speed )
  {
	res->count = (int) 2 * a ;
	res->value = -20.0 ;
  }
  else
  {
	res->count = 110 ;
	res->value = 0.0 ;
  }

  return res ;

}



// Ownership of the string transferred to the caller.
enum tur_status baz( unsigned int u, const char * m )
{

  if ( u == 10 )
	return tur_value ;

  return non_tur_value ;

}



bool tur()
{

  return true ;

}


// Ownership of the string *not* transferred to the caller.
char * frob( enum tur_status status )
{

  if ( status == tur_value )
	return "this is tur" ;

  return "this is non-tur" ;

}
