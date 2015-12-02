#' Validates and Reports If Logical
#' 
#' Validates and Reports If Logical
#' 
#' @param x character vector
#' @param colname_x vector's colname
#' @export
vc_logicial <- function(x, colname_x = "the column") {
	
	x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA
	
	
	is_logical <- x %in% c("true", "false", "TRUE", "FALSE", "T", "F") | is.na(x)
	are_logical <- all(is_logical|is.na(x))
	
	if (!are_credits ){
		message <- sprintf(
			"The following rows of %s do not follow the format of true/false:\n\n%s\n\n\n\n",	
			sQuote(colname_x)
			, paste(which(!is_logical)+1
					,collapse=", "))
		cat(message)
		
	}
	return(are_logical)
}
