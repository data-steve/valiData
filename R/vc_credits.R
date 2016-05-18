#' Validates and If Formatted Like Credit
#'
#' Validates and If Formatted Like Credit
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     credits = c(NA, 1.21, 1.34, '4.0', '5.0', .1, 12, 1111),
#'     stringsAsFactors = FALSE
#' )
#' vc_credits(dat, 'credits')
vc_credits <- function(data, x, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
	is_valid <-  stringi::stri_detect_regex(col, "^[0-9]{1,3}(\\.[0-9]{0,2})?$")# this could be tighter

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of %s do not follow the format of allowable credits:\n\n%s\n\n\n\n",
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
        call = 'vc_credits'
    )

    class(vc_output) <- 'vc'
    vc_output
}





