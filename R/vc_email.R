#' Validates and Reports If Email
#' 
#' Validates and Reports If Email
#' 
#' @param x character vector
#' @param colname_x vector's colname
#' @export

vc_email <- function(x, colname_x = "the column"){
	
	x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA
	
	original_na <- is.na(x)
	is_email <- grepl("([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", x, ignore.case = TRUE)
	are_emails <- all(is_email|is.na(x))
	
	if (!are_emails ){
		message <- sprintf(
			"The following rows of %s do not follow the format of allowable emails:\n\n%s\n\n\n\n",	
			sQuote(colname_x)
			, paste(which(!is_email & !original_na)+1
					,collapse=", "))
		cat(message)
		
	}
	return(are_emails)
	
}

	