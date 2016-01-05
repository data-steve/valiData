#' Validate that a CSV's Column Names Contain No Spaces
#'
#' \code{vt_spaced_colnames} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} column names contain no spaces.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param file.name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_spaced_colnames
#' @export
#' @examples
#' df <- mtcars; colnames(df)[c(1, 5)]  <- c("split header", "foo bar"); df
#' vt_spaced_colnames(df)
#' spaced_colnames_report(vt_spaced_colnames(df))
vt_spaced_colnames <- function(data, file.name = NULL){

    if (is.null(file.name)) file.name <- "The file"
	locs <- grep(" ", trimws(colnames(data)))

	list(
		valid = length(locs) == 0,                          ## logical did enough (proportion) elements validate
		locations =  colnames(data)[locs],
		call = "vt_spaced_colnames",                                         ## function name that was called
		file_name = file.name
	)
}

#' Validate that a CSV's Column Names Contain No Spaces
#'
#' \code{spaced_colnames_report} - Generates accomanying report.
#'
#' @param x A file or table validation function's (prefixed with \code{vf_} or
#' \code{vt_}) output.
#' @param \ldots ignored.
#' @rdname vt_spaced_colnames
#' @export
spaced_colnames_report <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {

		message <- sprintf(
			paste0(header("Column Order Test"),
				"'%s' appears to contain %s column names with spaces.\n\n",
                "Please remove white spaces from the following column headers:\n\n",
				"%s\n\n\n\n"
			),
			x[["file_name"]],
			length(x[["locations"]]),
			paste(paste0("\t -", x[["locations"]]) , collapse = "\n" )
		)

		class(message) <- c('invalid_report', "character")
		message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}

}

