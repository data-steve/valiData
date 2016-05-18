#' Validates If Integer
#'
#' Validates If Integer
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     vals = c('9.99.99', '22', '22.', '22.34', '22.234', '23,456', '12,23', 'dog', NA),
#'     vals2 = c('123', '123,456', NA, 1, 0, 0, 0, 0, ""),
#'     stringsAsFactors = FALSE
#' )
#' vc_integer(dat, 'vals')
#' vc_integer(dat, 'vals2')
vc_integer <- function(data, x, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
	regex <- "^(-*(\\d+|\\d{1,3}(?:,\\d{3})+)(\\.0*)?)$"
    is_valid <- stringi::stri_detect_regex(col, regex)

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of %s are not integer format:\n\n%s\n\n\n\n",
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
        call = 'vc_integer'
    )

    class(vc_output) <- 'vc'
    vc_output
}







































