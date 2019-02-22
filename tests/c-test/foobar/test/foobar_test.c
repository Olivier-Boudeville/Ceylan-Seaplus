/*
 * Copyright (C) 2018-2019 Olivier Boudeville
 *
 * This file is part of the Ceylan-Seaplus tests and examples.
 *
 * It has been placed in the public domain.
 *
 * Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
 *
 */


/*
 * Standalone tester of the Foobar service.
 *
 * Directly links to the foobar library.
 *
 */


// For the foobar API:
#include "foobar.h"


// For printf:
#include <stdio.h>

// For free:
#include <stdlib.h>



int main( int argc, const char* argv[] )
{

	printf( "\nTesting the foobar service (directly from C)\n\n" ) ;

	// Not needed in this context: foobar:start(),


	// myFooRecord = foobar:bar(3.14,full_speed),

	struct foo * my_foo = bar( 3.14, full_speed ) ;

	printf( "Count set to %i and value set to %f.\n",
			my_foo->count, my_foo->value ) ;


	// NewCount = foobar:foo(myFoo#foo.count),

	int new_count = foo( my_foo->count ) ;

	// Not needed anymore:
	free( my_foo ) ;

	printf( "Count now set to %i.\n", new_count ) ;

	enum tur_status res ;

	// Res = case foobar:tur() of
	if ( tur() )
	{

	  /* true ->
		   foobar:baz(NewCount,"Hello");
	   */
	  res = baz( new_count, "Hello" ) ;

	}
	else
	{

	  /* false ->
		   non_tur_value
	  */
	  res = non_tur_value ;
	}

	// Not needed in this context: foobar:stop(),

	printf( "Having: %s.\n", frob( res ) ) ;

	printf( "\nEnd of the foobar direct test.\n" ) ;

	return 0 ;

}
