#' Validates and Reports If ISO 8601
#'
#' Validates and Reports If ISO 8601
#'
#' @param x character vector
#' @param colname_x vector's colname
#' @export
vc_iso_datetime <- function(x, colname_x = "the column"){

# if (colname_x == "RegisteredDate") browser()

	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA

	original_na <- is.na(x)
	x[!is.na(x)] <- parsedate::parse_iso_8601(x[!is.na(x)])
	which_non_iso <- setdiff(which(original_na), which(is.na(x)))
	are_iso_datetimes <- all(length(which_non_iso)==0)

	if (!are_iso_datetimes ){
		message <- sprintf(
			"The following rows of %s do not follow the ISO 8601 date format:\n\n%s\n\n\n\n",
			sQuote(colname_x)
			, paste(which_non_iso+1
					,collapse=", "))
		cat(message)

	}
	return(are_iso_datetimes)

}



###
#parsedate::parse_iso_8601(x[8385:8385])
# parsedate::parse_iso_8601(x)
# head(x)


