#' Validates and Reports If Less Than
#'
#' Validates and Reports If Less Than
#'
#' @param x character vector to be coerced
#' @param y character vector to be coerced and compared
#' @param colnames X vector's colname
#' @param colname_y Y vector's colname

vc_less_than <- function(x, y, colnames = "the X column" , colname_y = "the Y column"){

	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA

	is_less_than <- x < y
	are_less_than <- all(is_less_than|is.na(x))

	if (!are_less_than ){
		message <- sprintf(
			"The following rows of %s are not less than %s:\n\n%s\n\n\n\n",
			sQuote(colnames)
			, sQuote(colname_y)
			, paste(which(!is_less_than )
					,collapse=", "))
		cat(message)

	}

	return(are_less_than)
}
