#' Validates and Reports If Numeric
#' 
#' Validates and Reports If Numeric
#' 
#' @param x character vector
#' @param colname_x vector's colname
#' @export
vc_numeric <- function(x, colname_x = "the column"){
	
	x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA
	
	which_numeric <- setdiff(is.na(x), is.na(as.numeric(x)))
	are_numeric <- all(length(which_numeric)==0)
	
	if (!are_numeric ){
		message <- sprintf(
			"The following rows of %s are not numeric format:\n\n%s\n\n\n\n",	
			sQuote(colname_x)
			, paste(which_numeric+1
					,collapse=", "))
		cat(message)
		
	}
	return(are_numeric)
	
}