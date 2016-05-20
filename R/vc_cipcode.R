#' Validates If Valid CIP Code
#'
#' Validates If Valid CIP Code
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @return Returns a \code{vc} classed list object.
#' @examples
#' dat <- data.frame(
#'     cips = c('9.99.99', '22', '22.', '22.34', '22.234', '22.2344', '22.2345', NA),
#'     stringsAsFactors = FALSE
#' )
#' vc_cipcode(dat, 'cips')
vc_cipcode <- function(data, x, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
    cipregex <- "(^0$)(*SKIP)(*FAIL)|((^\\s*\\d{1,2}\\.??\\s*$)|(^\\s*\\d{1,2}\\.(\\d{2,4})\\s*$))"
	is_valid <- grepl(cipregex, col, perl=TRUE)
	is_valid[is_na] <- NA

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of %s do not follow the format of a\nstandard (##. or ##.## or ##.####) CIP Code:\n\n%s\n\n\n\n",
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
        call = 'vc_cipcode'
    )

    class(vc_output) <- 'vc'
    vc_output
}
