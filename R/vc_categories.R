#' Validates and Reports If Correct Categories Used
#'
#' Validates and Reports If Correct Categories Used
#'
#' @param x character vector
#' @param levels levels of the category
#' @param colname_x vector's colname
#' @export
vc_categories <- function(x, levels = "the levels", colname_x = "the column"  ){

	x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA

	is_category <- tolower(x) %in% tolower(levels)
	are_categories <- all(is_category|is.na(x))

	if (!are_categories ){
		message <- sprintf(
			"The following rows of %s are not accepted categories:\n\n%s\n\n\n\n",
			sQuote(colname_x)
			, output_truncate(which(!is_category )+1))
		cat(message)

	}

	return(are_categories )
}
