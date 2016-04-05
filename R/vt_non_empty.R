#' Validate a CSV Contains Rows
#'
#' \code{vt_non_empty} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} contains rows.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param file.name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_non_empty
#' @export
#' @examples
#' vt_non_empty(CO2)
#' vt_non_empty(CO2[, 1:3])
vt_non_empty <- function(data, file.name = NULL) {

	if (is.null(file.name)) file.name <- "The file"

	list(
		valid = nrow(data) > 0,                          ## logical did enough (proportion) elements validate
		locations = NULL,    ## location of those not validating
		proportion = NULL,                  ## proportion of those vaidating
		call = "vt_non_empty",                        ## function name that was called
		file_name = file.name
	)

}



#' Validate a CSV Contains Rows
#'
#' \code{report_duplicated_rows} - Generates accomanying report.
#'
#' @param x A file or table validation function's (prefixed with \code{vf_} or
#' \code{vt_}) output.
#' @param \ldots ignored.
#' @rdname vt_non_empty
#' @export
non_empty_report <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {

		message <- sprintf(
			paste0(header("Duplicated Rows Test"),
				"'%s' appears to have no rows.  Please inspect this file and add 1 or more rows.\n\n\n\n"
			),
			x[["file_name"]]
		)
		class(message) <- c("invalid_report", "character")
		message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}


}

