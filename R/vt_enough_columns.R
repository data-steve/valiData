#' Validate that a CSV's has N Columns
#'
#' \code{vt_enough_columns} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} has n columns.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param file_name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_enough_columns
#' @export
#' @examples
#' set.seed(10)
#' map <- data.frame(
#'     header = colnames(mtcars)[-1],
#'     required = sample(c(TRUE, FALSE), -1 + ncol(mtcars), TRUE), stringsAsFactors = FALSE
#' )
#'
#' vt_enough_columns(mtcars, map)
#' str(vt_enough_columns(mtcars, map))
vt_enough_columns <- function(data, map, file_name = NULL){

    if (is.null(file_name)) file_name <- "The file"

	ncolsr <- list(
		valid = c(length(names(map[["column_level"]][[file_name]])) == ncol(data)),  ## logical did enough (proportion) elements validate
		locations = NULL,                        ## location of those not validating
		proportion = ncol(data)/nrow(map),       ## proportion of those vaidating
		call = "vt_enough_columns",                        ## function name that was called
		file_name = file_name,
		expected_ncols = length(names(map[["column_level"]][[file_name]])),
		actual_ncols = ncol(data)
	)
	class(ncolsr) <-'vt_enough_columns'
	ncolsr
}


#' Prints a vt_enough_columns Object
#'
#' Prints a vt_enough_columns object
#'
#' @param x A vt_enough_columns object.
#' @param \ldots ignored.
#' @method print vt_enough_columns
#' @export
print.vt_enough_columns <- function(x, ...){

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
	print(message)
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
	print(message)
	}

}
