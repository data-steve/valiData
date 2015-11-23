#' Validates and Reports If Formatted Like Regex Provided
#'
#' Validates and Reports If Formatted Like Regex Provided
#'
#' @param x character vector
#' @param regex regex
#' @param colnames vector's colname
#' @export
vc_regex <- function(x, regex , colnames = "the column"){

	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA

	original_na <- is.na(x)
	is_regex <- stringi::stri_detect_regex(x, regex)
	are_regex <- all(is_cregex|is.na(x))

	if (!are_credits ){
		message <- sprintf(
			"The following rows of %s do not follow the format provided:\n\n%s\n\n\n\n",
			sQuote(colnames)
			, paste(which(!is_regex & !original_na)
					,collapse=", "))
		cat(message)

	}

	return(are_regex)
}
