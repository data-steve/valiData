#' Validate that a CSV's Columns Are Correctly Ordered
#'
#' \code{vt_column_order} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} columns are ordered as expected.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param file_name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_column_order
#' @export
#' @examples
#' set.seed(10)
#' map <- data.frame(
#'     header = sample(colnames(mtcars)),
#'     required = sample(c(TRUE, FALSE), ncol(mtcars), TRUE), stringsAsFactors = FALSE
#' )
#'
#' vt_column_order(mtcars, map)
#' str(vt_column_order(mtcars, map))
vt_column_order <- function(data, map, file_name = NULL){

    if (is.null(file_name)) file_name <- "The file"

	exp_vs_act <- stats::setNames(data.frame(
		cbind_fill(
			names(map[["column_level"]][[file_name]]),
		    colnames(data)
	    ),
		stringsAsFactors = FALSE
	), c("Expected", "Actual"))

    exp_vs_act[["diff"]] <- apply(exp_vs_act, 1, function(x) {tolower(x[["Expected"]]) == tolower(x[["Actual"]])})
    exp_vs_act <- exp_vs_act[is.na(exp_vs_act[["diff"]]) | !exp_vs_act[["diff"]], c("Expected", "Actual")]

	colorder <- list(
		valid = nrow(exp_vs_act) == 0,                          ## logical did enough (proportion) elements validate
		locations =  exp_vs_act,
		call = "vt_column_order",                                         ## function name that was called
		file_name = file_name
	)
	class(colorder) <-'vt_column_order'
	colorder
}

cbind_fill <- function(...){
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function (x)
        rbind(x, matrix(, n-nrow(x), ncol(x)))))
}

#' Prints a vt_column_order Object
#'
#' Prints a vt_column_order object
#'
#' @param x A vt_column_order object.
#' @param \ldots ignored.
#' @method print vt_column_order
#' @export
print.vt_column_order <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {

		x[["locations"]][is.na(x[["locations"]])] <- "-"
		tab <- utils::capture.output(x[["locations"]])

		message <- sprintf(
			paste0(header("Column Order Test"),
				"'%s' appears to be out of order (order does not match the template order).\n\n",
                "This may be the result of:\n",
					" (1) Improper column order (match the template); \n",
		            " (2) Typos in header names; \n",
					" (3) Missing and/or additional columns;\n\n",
				"The following table shows the expected column header order compared to the supplied file:",
				"\n\n%s\n\n\n\n"
			),
			x[["file_name"]],
			paste(tab, collapse="\n")
		)

		class(message) <- c('invalid_report', "character")
		print(message)
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		print(message)
	}

}
