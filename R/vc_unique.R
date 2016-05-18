#' Validates and Reports If Unique
#'
#' Validates and Reports If Unique
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     id = as.character(c(1:5, NA, 1, 4, 10:14)),
#'     guid = c(1:12, NA)
#' )
#' vc_unique(dat, 'id')
#' vc_unique(dat, 'guid')
vc_unique <- function(data, x, regex, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
    is_valid <- !duplicated(col)
    is_valid[is_na] <- NA

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"%s contains %s duplicates.\nThe following rows are duplicates:\n\n%s\n\n\n\n",
			sQuote(x),
		    sum(!is_valid, na.rm =TRUE),
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
        call = 'vc_unique'
    )

    class(vc_output) <- 'vc'
    vc_output
}



