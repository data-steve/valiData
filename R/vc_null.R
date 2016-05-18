#' Checks For Null Elements In Vector
#'
#' Checks For Null Elements In Vector
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param prop.acceptable proportion of allowable missing.
#' @param missing values that missing can take.
#' @param required whether the vector in the table is required.
#' @param \dots ignored.
#' @export
#' @examples
#' set.seed(10)
#' dat <- data.frame(x = sample(c("NULL", rep(LETTERS[1:10])), 10000, TRUE))
#' vc_null(dat, 'x')
#' vc_null(dat, 'x', prop.acceptable = .1)
vc_null <- function(data, x, prop.acceptable = 0, missing = c("", "NULL"),
    required = FALSE, ...){

	## make sure prop.acceptable is 0-1 range
	stopifnot(prop.acceptable >= 0 && prop.acceptable <= 1)

    ## select the column & replace missing with NA
    col <- data[[x]]

	## replace potential missing values with NA
	col[trimws(col) %in% missing] <- NA

	## valid (valid if not missing)
	is_na <- is.na(col)
	is_valid <- !is_na

	## calculate the proportion missing
	prop_missing <- mean(is_na)

	#is column valid (less than = to prop acceptable)?
	are_valid <- prop_missing <= prop.acceptable

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"%s has %s%% of observations as NULL.  This is %s the accepted level of %s%%.\nThe following rows contain NULL values:\n\n%s\n\n\n\n",
			sQuote(x),
		    round(prop_missing, 3)*100,
		    ifelse(are_valid, "at or below", "above"),
		    round(prop.acceptable, 3)*100,
		    output_truncate(which(!(is_valid)))
		)
	} else {
	    message <- NULL
	}

    ## construct vc list & class
    vc_output <- list(
        column_name = x,
        valid = are_valid,
        message = message,
        passing = is_na,
        missing = is_na,
        call = 'vc_null',
        required = required
    )

    class(vc_output) <- 'vc'
    vc_output

}


