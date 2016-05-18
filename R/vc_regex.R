#' Validates and Reports If Formatted Like Regex Provided
#'
#' Validates and Reports If Formatted Like Regex Provided
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     vals = c('Greg', 'John', 'Tyrone', 'Susan', 'Kyra', 'Nells12', NA, ""),
#'     stringsAsFactors = FALSE
#'  )
#' vc_regex(dat, 'vals', '^[A-Z][^0-9]+$')
#' vc_regex(dat, 'vals', '^[A-Z].+$')
vc_regex <- function(data, x, regex, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
	is_valid <- stringi::stri_detect_regex(col, regex)
    is_valid[is_na] <- NA

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of %s do not follow the format (%s) provided:\n\n%s\n\n\n\n",
			sQuote(x),
		    regex,
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
        call = 'vc_regex'
    )

    class(vc_output) <- 'vc'
    vc_output
}




