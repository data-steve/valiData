#' Validates If Email
#'
#' Validates If Email
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     email =c('cookie@cookiemonster.com', 'joe@hometown.uni'
#'     , 'meet@seven', '@Jim', 'joe@gmail.com', NA),
#'     stringsAsFactors = FALSE
#' )
#' vc_email(dat, 'email')
vc_email <- function(data, x, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
    regex <- "^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}$"
	is_valid <- grepl(regex, col, ignore.case = TRUE)
	is_valid[is_na] <- NA

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of %s do not follow the format of allowable emails:\n\n%s\n\n\n\n",
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
        call = 'vc_email'
    )

    class(vc_output) <- 'vc'
    vc_output
}

