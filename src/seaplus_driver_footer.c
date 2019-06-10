
	default:
	  raise_error( "Unknown function identifier: %u", current_fun_id ) ;

	}

	finalize_command_after_writing( &output_sm_buf ) ;

  }

  // output_sm_buf internally already freed appropriately.

  stop_seaplus_driver( read_buf ) ;

}
