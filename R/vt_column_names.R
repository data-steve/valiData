#' Validate that a CSV's Columns Are Named Correctly
#'
#' \code{vt_column_names} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} columns are named correctly.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param ignore.case logical.  Should case be ignored?
#' @param ignore.space logical.  Should whitespace be ignored?
#' @param file_name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_column_names
#' @export
vt_column_names <- function(data, map, ignore.case = FALSE, ignore.space = FALSE, file_name = NULL){

    if (is.null(file_name)) file_name <- "The file"


	act_nms <- actual_names <- colnames(data)
	exp_nms <- expected_names <- names(map[["column_level"]][[file_name]])

	if (ignore.case) {
		actual_names <- tolower(actual_names)
		expected_names <- tolower(expected_names)
	}

	if (ignore.space) {
		actual_names <- gsub("\\s+", "", actual_names)
		expected_names <- gsub("\\s+", "", expected_names)
	}

	not_found <- setdiff(expected_names, actual_names)
	not_found <- exp_nms[expected_names %in% not_found]

	not_expected <- setdiff(actual_names, expected_names)
	not_expected <- act_nms[actual_names %in% not_expected]

	nullify <- function(x) {if(length(x) == 0) NULL else x}

	colnms <- list(
		valid = length(not_found) == 0 ,                          ## logical did enough (proportion) elements validate
		locations =  list(                                            ## location of those not validating
			missing_headers = nullify(not_found),
			unexpected_headers = nullify(not_expected)
		),
		proportion =  1 - length(not_found)/length(expected_names),   ## proportion of those vaidating
		call = "vt_column_names",                                         ## function name that was called
		file_name = file_name,
		ignore_case = ignore.case,
		ignore_space = ignore.space
	)
	class(colnms) <- 'vt_column_names'
	colnms
}


#' Prints a vt_column_names Object
#'
#' Prints a vt_column_names object
#'
#' @param x A vt_column_names object.
#' @param \ldots ignored.
#' @method print vt_column_names
#' @export
print.vt_column_names <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {

		if (length(x[["locations"]][["unexpected_headers"]]) > 0) {
			additional2 <- "Additionally, the following columns were present but not expected:\n\n"
			additional2 <- paste0(paste(paste0("\t- ", x[["locations"]][["unexpected_headers"]]), collapse = "\n"), '\n\n\n\n')
		} else {
			additional2 <- ""
		}

		message <- sprintf(
			paste0(header("Column Names Test"),
				"'%s' has column names that do not match the expected template column names.\n",
				"The following column names are not present in '%s' but expected:\n\n%s\n\n\n\n",
				"%s"
			),
			x[["file_name"]],
			#additional,
			x[["file_name"]],
	        paste(paste0("\t- ", x[["locations"]][["missing_headers"]]), collapse = "\n"),
			additional2
		)

		class(message) <- c('invalid_report', "character")
		print(message)
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		print(message)
	}

}
