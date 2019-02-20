
#ifndef _SEAPLUS_H_
#define _SEAPLUS_H_


#include "erl_interface.h"

typedef unsigned char byte;

// Forward declarations:

int read_command( byte *buf ) ;
int write_command (byte *buf, int len ) ;
int read_exact( byte *buf, int len ) ;
int write_exact( byte *buf, int len ) ;


typedef unsigned int tuple_index ;

// No forward declaration seems possible:
//struct ETERM ;

// So:

// For ETERM and all:
#include "erl_interface.h"

byte * start_driver() ;
void stop_driver( byte * buffer ) ;

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
