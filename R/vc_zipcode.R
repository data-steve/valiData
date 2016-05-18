#' Validates If ZipCode
#'
#' Validates If ZipCode
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     zip1 = c('14222', '14222-1234', '123456', 'dog', '', 'NULL', 'NA'),
#'     zip2 = c('14222', '14222-1234', '12345', 'n/a', '', 'NULL', 'NA'),
#'     stringsAsFactors = FALSE
#' )
#' vc_zipcode(dat, 'zip1')
#' vc_zipcode(dat, 'zip2')
vc_zipcode <- function(data, x, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
	regex <- "^\\d{5}(-\\d{4})?$"

    is_valid <- stringi::stri_detect_regex(col, regex)

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of %s do not follow the format of allowable zipcodes:\n\n%s\n\n\n\n",
			sQuote(x),
		    output_truncate(which(!(is_valid|is_na)))
		)
	} else {
	    message <- NULL
	}

    ## construct vc list & class
    vc_output <- list(
        column_name = x,
        valid = are_valid,
        message = message,
        passing = is_valid,
        missing = is_na,
        call = 'vc_zipcode'
    )

    class(vc_output) <- 'vc'
    vc_output
}


