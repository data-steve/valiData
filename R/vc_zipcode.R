#' Validates and Reports If ZipCode
#' 
#' Validates and Reports If ZipCode
#' 
#' @param x character vector
#' @param colnames vector's colname
#' @export

vc_zipcode <- function(x, colnames = "the column"){
	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA
	
	original_na <- is.na(x)
	is_zip <- grepl("\\d{5}(-\\d{4})?", x)
	are_zips <- all(is_zip|is.na(x))
	
	if (!are_zips ){
		message <- sprintf(
			"The following rows of %s do not follow the format of allowable zipcodes:\n\n%s\n\n\n\n",	
			sQuote(colnames)
			, paste(which(!is_zip & !original_na)
					,collapse=", "))
		cat(message)
		
	}
	return(are_zips)
	
}