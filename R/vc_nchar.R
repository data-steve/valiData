#' Validates Number of Characters
#'
#' Validates Number of Characters
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param nchar The expected max number of characters.
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     x = c('dog', 'cat', 'chicken', 'a', NA),
#'     stringsAsFactors = FALSE
#'  )
#' vc_nchar(dat, 'x', 3)
#' vc_nchar(dat, 'x', 10)
vc_nchar <- function(data, x, nchar, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
	is_valid <- nchar(col, keepNA=TRUE) <= nchar

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of column %s are not less than or equal to %s characters long:\n\n%s\n\n\n\n",
			sQuote(x),
		    nchar,
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
        call = 'vc_nchar'
    )

    class(vc_output) <- 'vc'
    vc_output
}
