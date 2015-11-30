#' Validates and Reports If Greater Than
#'
#' Validates and Reports If Greater Than
#'
#' @param x character vector to be coerced
#' @param y character vector to be coerced and compared
#' @param colnames X vector's colname
#' @param colname_y Y vector's colname

vc_greater_than <- function(x, y, colname_x = "the X column" , colname_y = "the Y column"){

	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA

	is_greater_than <- x > y
	are_greater_than <- all(is_greater_than|is.na(x))

	if (!are_greater_than ){
		message <- sprintf(
			"The following rows of %s are not greater than %s:\n\n%s\n\n\n\n",
			sQuote(colname_x)
			, sQuote(colname_y)
			, paste(which(!is_greater_than )
					,collapse=", "))
		cat(message)

	}

	return(are_greater_than)

}
