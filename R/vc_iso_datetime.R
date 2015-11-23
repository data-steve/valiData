#' Validates and Reports If ISO 8601
#'
#' Validates and Reports If ISO 8601
#'
#' @param x character vector
#' @param colnames vector's colname
#' @export
vc_iso_datetime <- function(x, colnames = "the column"){

# if (colnames == "RegisteredDate") browser()

	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA

	original_na <- is.na(x)
	x[!is.na(x)] <- parsedate::parse_iso_8601(x[!is.na(x)])
	which_non_iso <- setdiff(which(original_na), which(is.na(x)))
	are_iso_datetimes <- all(length(which_non_iso)==0)

	if (!are_iso_datetimes ){
		message <- sprintf(
			"The following rows of %s do not follow the ISO 8601 date format:\n\n%s\n\n\n\n",
			sQuote(colnames)
			, paste(which_non_iso
					,collapse=", "))
		cat(message)

	}
	return(are_iso_datetimes)

}



###
#parsedate::parse_iso_8601(x[8385:8385])
# parsedate::parse_iso_8601(x)
# head(x)


