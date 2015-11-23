#' Validate that a CSV's has N Columns
#'
#' \code{vt_ncols} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} has n columns.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param file.name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_ncols
#' @export
#' @examples
#' set.seed(10)
#' map <- data.frame(
#'     header = colnames(mtcars)[-1],
#'     required = sample(c(TRUE, FALSE), -1 + ncol(mtcars), TRUE), stringsAsFactors = FALSE
#' )
#'
#' vt_ncols(mtcars, map)
#' ncols_report(vt_ncols(mtcars, map))
vt_ncols <- function(data, map, file.name = NULL){

    if (is.null(file.name)) file.name <- "The file"
    stopifnot(all(c("header", "required") %in% colnames(map)))

	list(
		valid = c(nrow(map) == ncol(data)),  ## logical did enough (proportion) elements validate
		locations = NULL,                        ## location of those not validating
		proportion = ncol(data)/nrow(map),       ## proportion of those vaidating
		call = "vt_ncols",                        ## function name that was called
		file_name = file.name,
		expected_ncols = nrow(map),
		actual_ncols = ncol(data)
	)
}


#' Validate that a CSV's has N Columns
#'
#' \code{ncols_report} - Generates accomanying report.
#'
#' @param x A file or table validation function's (prefixed with \code{vf_} or
#' \code{vt_}) output.
#' @param \ldots ignored.
#' @rdname vt_ncols
#' @export
ncols_report <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {

	message <- sprintf(
		paste0(header("Number of Columns Test"),
			"'%s' does not have %s columns as expected. %s has %s columns.\n\n",
			"To determine the %s columns, please:\n",
			" (1) Compare the upload headers against the template headers;\n",
			" (2) Consult the Column Names Report in the next section. \n\n\n\n"
		),
		x[["file_name"]],
		x[["expected_ncols"]],
		x[["file_name"]],
        x[["actual_ncols"]],
		ifelse(x[["proportion"]] > 1, "extra", "missing")
	)

	class(message) <- c("invalid_report", "character")
	message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}

}
