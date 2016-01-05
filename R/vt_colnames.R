#' Validate that a CSV's Columns Are Named Correctly
#'
#' \code{vt_colnames} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} columns are named correctly.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param ignore.case logical.  Should case be ignored?
#' @param ignore.space logical.  Should whitespace be ignored?
#' @param file.name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_colnames
#' @export
#' @examples
#' set.seed(10)
#' map <- data.frame(
#'     header = c("foobar", colnames(mtcars)[-1]),
#'     required = sample(c(TRUE, FALSE), ncol(mtcars), TRUE), stringsAsFactors = FALSE
#' )
#'
#' vt_colnames(mtcars, map)
#' colnames_report(vt_colnames(mtcars, map))
vt_colnames <- function(data, map, ignore.case = FALSE, ignore.space = FALSE, file.name = NULL){

    if (is.null(file.name)) file.name <- "The file"
    stopifnot(all(c("header", "required") %in% colnames(map)))

	act_nms <- actual_names <- colnames(data)
	exp_nms <- expected_names <- map[["header"]]

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

	list(
		valid = length(not_found) == 0 ,                          ## logical did enough (proportion) elements validate
		locations =  list(                                            ## location of those not validating
			missing_headers = nullify(not_found),
			unexpected_headers = nullify(not_expected)
		),
		proportion =  1 - length(not_found)/length(expected_names),   ## proportion of those vaidating
		call = "vt_colnames",                                         ## function name that was called
		file_name = file.name,
		ignore_case = ignore.case,
		ignore_space = ignore.space
	)
}


#' Validate that a CSV's Columns Are Named Correctly
#'
#' \code{colnames_report} - Generates accomanying report.
#'
#' @param x A file or table validation function's (prefixed with \code{vf_} or
#' \code{vt_}) output.
#' @param \ldots ignored.
#' @rdname vt_colnames
#' @export
colnames_report <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {

# changed message below per consultants request
	    # we are no longer rejecting imports based on
	    # case-sensitivity 12/2/15 Tyler

# 		if (isTRUE(x[["ignore_case"]])|isTRUE(x[["ignore_space"]])) {
# 			ignores <- gsub("(^ & )|( & $)", "", paste(
# 				ifelse(x[["ignore_case"]], "capitalization", ""),
# 				ifelse(x[["ignore_space"]],"white space", ""),
# 				sep=" & "
# 			))
# 			additional <- sprintf("Ignoring %s t", ignores)
# 		} else {
# 			additional <- "T"
# 			ignores <- NULL
# 		}

# 		if (length(x[["locations"]][["unexpected_headers"]]) > 0) {
# 			additional2 <- sprintf(
# 				"Additionally, %sthe following columns were present but not expected:\n\n",
# 				ifelse(is.null(ignores), "", gsub(" t$", ",", tolower(additional)))
# 			)
# 		} else {
# 			additional2 <- ""
# 		}
		if (length(x[["locations"]][["unexpected_headers"]]) > 0) {
			additional2 <- "Additionally, the following columns were present but not expected:\n\n"
		} else {
			additional2 <- ""
		}

# 		message <- sprintf(
# 			paste0(header("Column Names Test"),
# 				"'%s' has column names that do not match the expected template column names.\n",
# 				"%she following column names are not present in %s but expected:\n\n%s\n\n\n\n",
# 				"%s%s\n\n\n\n"
# 			),
# 			x[["file_name"]],
# 			additional,
# 			x[["file_name"]],
# 	        paste(paste0("\t- ", x[["locations"]][["missing_headers"]]), collapse = "\n"),
# 			additional2,
# 	        paste(paste0("\t- ", x[["locations"]][["unexpected_headers"]]), collapse = "\n")
# 		)

		message <- sprintf(
			paste0(header("Column Names Test"),
				"'%s' has column names that do not match the expected template column names.\n",
				"The following column names are not present in %s but expected:\n\n%s\n\n\n\n",
				"%s%s\n\n\n\n"
			),
			x[["file_name"]],
			#additional,
			x[["file_name"]],
	        paste(paste0("\t- ", x[["locations"]][["missing_headers"]]), collapse = "\n"),
			additional2,
	        paste(paste0("\t- ", x[["locations"]][["unexpected_headers"]]), collapse = "\n")
		)


		class(message) <- c('invalid_report', "character")
		message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}

}
