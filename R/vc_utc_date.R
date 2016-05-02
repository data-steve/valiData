#' Validates and Reports If Date UTC
#'
#' Validates and Reports If Date UTC
#'
#' @param x character vector
#' @param colname_x vector's colname
#' @export

vc_utc_date <- function(x, colname_x = "the column"){

	x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA

	original_na <- is.na(x)
	x[!original_na] <- parsedate::parse_date(trimws(x[!is.na(x)]))
	locs <- which_non_utc <- setdiff(which(is.na(x)), which(original_na)) #Tyler switched order of these args on 12/2/15
	are_utc_dates <- all(length(which_non_utc)==0)

	if (length(locs) > 100) {
	    locs <- paste0(paste(locs[1:100]+1, collapse=", "), "...[truncated]...")
	} else {
	    locs <- paste(locs+1, collapse=", ")
	}

	if (!are_utc_dates){
		message <- sprintf(
			"%s contains %s rows that do not follow the UTC date format:\n\n%s\n\n\n\n",
			sQuote(colname_x),
		    length(which_non_utc),
			output_truncate(locs))
		cat(message)

	}
	return(are_utc_dates)

}
