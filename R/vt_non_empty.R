#' Validate a CSV Contains Rows
#'
#' \code{vt_non_empty} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} contains rows.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param file.name An optional file name for use in reporting.
#' @param \ldots ignored.
#' @return Returns a list of validation results.
#' @rdname vt_non_empty
#' @export
#' @examples
#' vt_non_empty(CO2)
#' vt_non_empty(CO2[0, 1:3])
#' str(vt_non_empty(CO2[0, 1:3]))
vt_non_empty <- function(data, file.name = NULL, ...) {

	if (is.null(file.name)) file.name <- "The file"

	non_empty <- list(
		valid = nrow(data) > 0,                          ## logical did enough (proportion) elements validate
		locations = NULL,    ## location of those not validating
		proportion = NULL,                  ## proportion of those vaidating
		call = "vt_non_empty",                        ## function name that was called
		file_name = file.name
	)
    class(non_empty) <- 'vt_non_empty'
    non_empty
}



#' Prints a vt_non_empty  Object
#'
#' Prints a vt_non_empty  object
#'
#' @param x A vt_non_empty  object.
#' @param \ldots ignored.
#' @method print vt_non_empty
#' @export
print.vt_non_empty <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {

		message <- sprintf(
			paste0(header("Table Not Empty"),
				"'%s' appears to have no rows.  Please inspect this file and add 1 or more rows.\n We cannot run any further tests on this file until you do.\n\n\n\n"
			),
			x[["file_name"]]
		)
		class(message) <- c("invalid_report", "character")
		print(message)
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		print(message)
	}


}

