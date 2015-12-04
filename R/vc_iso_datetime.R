#' Validates and Reports If ISO 8601
#'
#' Validates and Reports If ISO 8601
#'
#' @param x character vector
#' @param colname_x vector's colname
#' @export
vc_iso_datetime <- function(x, colname_x = "the column"){


	x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA

	original_na <- is.na(x)
	x[!is.na(x)] <- parsedate::parse_iso_8601(trimws(x[!is.na(x)]))
	locs <- which_non_iso <- setdiff(which(is.na(x)), which(original_na)) #Tyler switched order of these args on 12/2/15
	are_iso_datetimes <- all(length(which_non_iso)==0)

	if (length(locs) > 100) {
	    locs <- paste0(paste(locs[1:100]+1, collapse=", "), "...[truncated]...")
	} else {
	    locs <- paste(locs+1, collapse=", ")
    }

	if (!are_iso_datetimes ){
		message <- sprintf(
			"%s contains %s rows that do not follow the ISO 8601 date format:\n\n%s\n\n\n\n",
			sQuote(colname_x),
		    length(which_non_iso),
			locs)
		cat(message)

	}
	return(are_iso_datetimes)

}



###
#parsedate::parse_iso_8601(x[8385:8385])
# parsedate::parse_iso_8601(x)
# head(x)


