/* Standalone tester of the Foobar service.
 *
 */


// For printf:
#include <stdio.h>

// For free:
#include <stdlib.h>

// For service API:
#include "foobar.h"


int main( int argc, const char* argv[] )
{

	printf( "\nTesting the foobar service\n\n" ) ;

	// Not needed in this context: foobar:start(),


	// myFooRecord = foobar:bar(3.14,full_speed),

	struct s * my_s = bar( 3.14, full_speed ) ;

	printf( "Count set to %i and value set to %f.\n",
			my_s->count, my_s->value ) ;


	// NewCount = foobar:foo(myFooRecord#some_foo_record.count),

	int new_count = foo( my_s->count ) ;

	// Not needed anymore:
	free( my_s ) ;

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
		   non_tur
	  */
	  res = non_tur_value ;
	}

	// Not needed in this context: foobar:stop(),

	printf( "Having: %s.\n", frob( res ) ) ;

	return 0 ;

}
