#' Generic Column Validation
#'
#' Generic column validation
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param expr An string that can be evaluated elementwise as an expression to
#' test on \code{x} column (include column \code{x} as seen in Examples below).
#' @param \dots ignored.
#' @export
#' @returns Returns a \code{vc} classed list object.
#' @examples
#' vc_(mtcars, 'mpg', 'abs(scale(mpg)) < 2')
#' vc_(mtcars, 'mpg', 'abs(scale(mpg)) < 2',
#'     augment_comment = "...\n    because they may be (outliers > 2 sd)")
#' vc_(CO2, 'Type', '!grepl("^M", Type)',
#'     augment_comment = " because they don't begin with `M`")
#' vc_(mtcars, 'drat', 'drat >= wt',
#'     augment_comment = " because `drat` is less than `wt`")
#' str(vc_(mtcars, 'mpg', 'abs(scale(mpg)) < 2'))
vc_ <- function(data, x, expr, augment_comment = NULL, ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- c(is.na(col))

    ## expression to validate against (elementwise)
	is_valid <- c(eval(parse(text=expr), data, enclos = parent.frame()))
    is_valid[is_na] <- NA

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of %s are not valid%s:\n\n%s\n\n\n\n",
			sQuote(x),
		    ifelse(is.null(augment_comment), "", augment_comment),
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
        call = 'vc_'
    )

    class(vc_output) <- 'vc'
    vc_output
}
