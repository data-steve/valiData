#' Validates If Date UTC
#'
#' Validates If Date UTC
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param \dots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     start = c("2016-01-01T09", "R2D2-3CPO", sprintf("2016-04-%sT09", 12:19), NA),
#'     end = c(NA, sprintf("2016-01-%sT09", 11:20)),
#'     stringsAsFactors = FALSE
#' )
#'
#' vc_utc_date(dat, 'start')
#' vc_utc_date(dat, 'end')
vc_utc_date <- function(data, x, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
    col[!is_na] <- parsedate::parse_iso_8601(trimws(col[!is_na]))
    is_valid <- !c(is.na(col) & !is_na)

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"%s contains %s rows that do not follow the UTC date format:\n\n%s\n\n\n\n",
			sQuote(x),
		    length(!(is_valid|is_na)),
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
        call = 'vc_utc_data'
    )

    class(vc_output) <- 'vc'
    vc_output
}

