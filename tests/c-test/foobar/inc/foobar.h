
/*
 * Declaration of the C API:
 *
 * Not an element that Seaplus or the service integrator control!
 *
 */

// For bool:
#include <stdbool.h>


// Type declarations.

// We need more than a forward declaration like 'struct s ;':
struct s
{

  int count ;
  float value ;

} ;

enum foo_status { low_speed, moderate_speed, full_speed } ;

enum tur_status { tur_value, non_tur_value } ;



// API declaration.

int foo( int a ) ;
struct s * bar( double a, enum foo_status status ) ;
enum tur_status baz( unsigned int u, const char * m ) ;
bool tur() ;
char * frob( enum tur_status ) ;
