#' Validates and Reports If Valid CIP Code
#'
#' Validates and Reports If Valid CIP Code
#'
#' @param x character vector
#' @param colname_x vector's colname
vc_cipcode <- function (x, colname_x = "the column"){

	x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA

	original_na <- is.na(x)
    #cipregex <- "(^\\s*\\d{2}\\.??\\s*$)|(^\\s*\\d{2}\\.(\\d{2}|\\d{4})\\s*$)"
    cipregex <- "(^0$)(*SKIP)(*FAIL)|((^\\s*\\d{1,2}\\.??\\s*$)|(^\\s*\\d{1,2}\\.(\\d{2,4})\\s*$))"
	is_cipcode <- grepl(cipregex, x, perl=TRUE)

	are_cipcode <- all(is_cipcode|original_na)

	if (!are_cipcode){
		message <- sprintf(
			"The following rows of %s do not follow the format of a\nstandard (##. or ##.## or ##.####) CIP Code:\n\n%s\n\n\n\n",
			sQuote(colname_x)
			, output_truncate(which(!is_cipcode & !original_na)))
		cat(message)
	}

	return(are_cipcode)

}
