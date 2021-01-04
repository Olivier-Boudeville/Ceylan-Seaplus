/*
 * Copyright (C) 2018-2021 Olivier Boudeville
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


/* Simple test to ensure that the Seaplus library can be linked to with no
 * problem.
 *
 */

#include "seaplus.h"


int main()
{

  const char * log_filename = "seaplus_test.log" ;

  printf( "Simple test of the Seaplus library, see '%s' for more information.",
	log_filename ) ;

  start_logging( log_filename ) ;

  log_debug( "Hello!" ) ;

  log_debug( "Hello %s!", "there" ) ;

  log_trace( "Some trace." ) ;

  log_trace( "With two numbers: %i and %i.", 42, 0 ) ;

  stop_logging() ;

}
