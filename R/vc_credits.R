#' Validates and Reports If Formatted Like Credit
#'
#' Validates and Reports If Formatted Like Credit
#'
#' @param x character vector
#' @param colname_x vector's colname
#' @export
vc_credits <- function(x, colname_x = "the column"){

	x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA

	original_na <- is.na(x)
	is_credit <- stringi::stri_detect_regex(x,"[0-9]{1,2}\\.?[0-9]{0,2}")
	are_credits <- all(is_credit|original_na)

	if (!are_credits ){
		message <- sprintf(
			"The following rows of %s do not follow the format of allowable credits:\n\n%s\n\n\n\n",
			sQuote(colname_x)
			, paste(which(!is_credit & !original_na)+1
					,collapse=", "))
		cat(message)

	}

	return(are_credits)
}
