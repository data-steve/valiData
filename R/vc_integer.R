#' Validates and Reports If Integer
#' 
#' Validates and Reports If Integer
#' 
#' @param x character vector
#' @param colname_x vector's colname
#' @export
vc_integer <- function(x, colname_x = "the column"){
	
	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA
	
	which_integer <- setdiff(is.na(x), is.na(as.integer(x)))
	are_integer <- all(length(which_integer)==0)
	
	if (!are_integer){
		message <- sprintf(
			"The following rows of %s are not integer format:\n\n%s\n\n\n\n",	
			sQuote(colname_x)
			, paste(which_integer
					,collapse=", "))
		cat(message)
		
	}
	return(are_integer)
	
}