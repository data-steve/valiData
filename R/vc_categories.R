#' Validates If Correct Categories Used
#'
#' Validates If Correct Categories Used
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param levels Levels of the category.
#' @param \dots ignored.
#' @export
#' @returns Returns a \code{vc} classed list object.
#' @examples
#' vc_categories(mtcars, 'cyl', c(6, 8))
#' str(vc_categories(mtcars, 'cyl', c(6, 8)))
vc_categories <- function(data, x, levels = "the levels", ...){

    ## select the column & replace missing with NA
    col <- sub_out_missing(data[[x]])

    ## record missing (NA)
    is_na <- is.na(col)

    ## expression to validate against (elementwise)
	is_valid <- tolower(col) %in% tolower(levels)  # note ignores case
    is_valid[is_na] <- NA

	## valid columnwise: Are all elelemnts either valid or NA?
	are_valid <- all(is_valid|is_na)

	## generate the comment
	if (!are_valid){
		message <- sprintf(
			"The following rows of %s are not accepted categories:\n\n%s\n\n\n\n",
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
        call = 'vc_categories'
    )

    class(vc_output) <- 'vc'
    vc_output
}





