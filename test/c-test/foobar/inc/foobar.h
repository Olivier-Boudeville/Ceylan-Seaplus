/*
 * Copyright (C) 2018-2023 Olivier Boudeville
 *
 * This file is part of the Ceylan-Seaplus tests and examples.
 *
 * It has been placed in the public domain.
 *
 * Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
 *
 */


/*
 * Declaration of the C API:
 *
 * Not an element that Seaplus or the service integrator control!
 *
 */

// For bool:
#include <stdbool.h>


// Type declarations.

// We need more than a forward declaration like 'struct foo_data ;':
struct foo_data
{

  // Some comment about count:
  int count ;

  // Some comment about value:
  float value ;

} ;


// Detailed comment for foo_status.
enum foo_status { low_speed, moderate_speed, full_speed } ;


// Detailed comment for tur_status.
enum tur_status { tur_value, non_tur_value } ;



// API declaration.


// Detailed comment for foo.
int foo( int a ) ;


// Detailed comment for bar.
struct foo_data * bar( double a, enum foo_status status ) ;


// Detailed comment for baz.
enum tur_status baz( unsigned int u, const char * m ) ;


// Detailed comment for tur.
bool tur() ;


// Detailed comment for frob.
char * frob( enum tur_status ) ;
