
	default:
	  raise_error( "Unknown function identifier: %u", current_fun_id ) ;

	}

	clean_up_command( call_term, parameters ) ;

  }

  stop_seaplus_driver( buffer ) ;

}
