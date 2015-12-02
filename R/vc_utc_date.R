#' Validates and Reports If Date UTC
#' 
#' Validates and Reports If Date UTC
#' 
#' @param x character vector
#' @param colname_x vector's colname
#' @export

vc_utc_date <- function(x, colname_x = "the column"){
	
	x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA
	# as long as we can parse, we're fine

	original_na <- is.na(x)
	x[!original_na] <- parsedate::parse_date(x[!original_na])
	which_non_utc <- setdiff(which(original_na), which(is.na(x)))
	are_utc_dates <- all(length(which_non_utc)==0)
	
	if (!are_utc_dates){
		message <- sprintf(
			"The following rows of %s do not follow the UTC date format:\n\n%s\n\n\n\n",	
			sQuote(colname_x)
			, paste(which_non_utc+1
					,collapse=", "))
		cat(message)
		
	}
	return(are_utc_dates)
	
}