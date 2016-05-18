#' Validates If Logical
#'
#' Validates If Logical
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     check = c('T', 'TRUE', 'FALSE', 'F', 'false', 'true', NA, TRUE, FALSE, 'TURE'),
#'     stringsAsFactors = FALSE
#'  )
#' vc_logical(dat, 'check')
vc_logical <- function(data, x, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
	is_valid <- col %in% c("true", "false", "TRUE", "FALSE", "T", "F")
    is_valid[is_na] <- NA

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of %s do not follow the format of true/false:\n\n%s\n\n\n\n",
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
        call = 'vc_logical'
    )

    class(vc_output) <- 'vc'
    vc_output
}





