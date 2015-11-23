#' Validates and Reports If Logical
#' 
#' Validates and Reports If Logical
#' 
#' @param x character vector
#' @param colnames vector's colname
#' @export
vc_logicial <- function(x, colnames = "the column") {
	
	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA
	
	
	is_logical <- x %in% c("true", "false", "TRUE", "FALSE", "T", "F") | is.na(x)
	are_credits <- all(is_credit|is.na(x))
	
	if (!are_credits ){
		message <- sprintf(
			"The following rows of %s do not follow the format of allowable credits:\n\n%s\n\n\n\n",	
			sQuote(colnames)
			, paste(which(!is_credit)
					,collapse=", "))
		cat(message)
		
	}
	return(are_credits)
}
