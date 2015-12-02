vc_cipcode <- function (x, colname_x = "the column"){
	
	x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA
	
	original_na <- is.na(x)
	is_cipcode <- stringi::stri_detect_regex(x, "\\d{2}\\.\\d{4}")
	are_cipcode <- all(is_cipcode|original_na)
	
	
	if (!are_cipcode){
		message <- sprintf(
			"The following rows of %s do not follow the format of a standard CIP Code:\n\n%s\n\n\n\n",	
			sQuote(colname_x)
			, paste(which(!is_cipcode & !original_na)+1
					,collapse=", "))
		cat(message)	
	}
	
	return(are_cipcode)
	
} 