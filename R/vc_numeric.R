#' Validates and Reports If Numeric
#' 
#' Validates and Reports If Numeric
#' 
#' @param x character vector
#' @param colnames vector's colname
#' @export
vc_numeric <- function(x, colnames = "the column"){
	
	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA
	
	which_numeric <- setdiff(is.na(x), is.na(as.numeric(x)))
	are_numeric <- all(length(which_numeric)==0)
	
	if (!are_numeric ){
		message <- sprintf(
			"The following rows of %s are not numeric format:\n\n%s\n\n\n\n",	
			sQuote(colnames)
			, paste(which_numeric
					,collapse=", "))
		cat(message)
		
	}
	return(are_numeric)
	
}