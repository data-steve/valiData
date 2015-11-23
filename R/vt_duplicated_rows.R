#' Validate a CSV Contains No Duplicate Rows
#'
#' \code{vt_duplicated_rows} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} contains no duplicated rows.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param file.name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_duplicated_rows
#' @export
#' @examples
#' vt_duplicated_rows(CO2)
#' vt_duplicated_rows(CO2[, 1:3])
#' duplicated_rows_report(vt_duplicated_rows(CO2[, 1:3]))
vt_duplicated_rows <- function(data, file.name = NULL) {

	if (is.null(file.name)) file.name <- "The file"
	dups <- duplicated(data)

	if (sum(dups)>0){

		prop <- sum(dups)/nrow(data)
		loc <- which(dups)

	} else {

		prop <- NULL
		loc <- NULL
	}

	list(
		valid = sum(dups) == 0,                          ## logical did enough (proportion) elements validate
		locations = loc,    ## location of those not validating
		proportion = prop,                  ## proportion of those vaidating
		call = "vt_duplicated_rows",                        ## function name that was called
		file_name = file.name
	)

}



#' Validate a CSV Contains No Duplicate Rows
#'
#' \code{report_duplicated_rows} - Generates accomanying report.
#'
#' @param x A file or table validation function's (prefixed with \code{vf_} or
#' \code{vt_}) output.
#' @param \ldots ignored.
#' @rdname vt_duplicated_rows
#' @export
duplicated_rows_report <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {

		locs <- x[["locations"]]
		if (length(locs) > 100) {
			locs <- locs[1:100]
			truncmess <- " (truncated to first 100 elements)"
			locs <- paste0(paste(locs, collapse=", "), "...[truncated]...")
		} else {
			truncmess <- ""
			locs <- paste(locs, collapse=", ")
		}
		message <- sprintf(
			paste0(header("Duplicated Rows Test"),
				"'%s' appears to have %s duplicated rows.\n",
				"This is often the result of not using unique IDs/GUIDs or a data entry error.\n\n",
				"These suggestions are likey to fix the problem:\n",
					" (1) Provide unique IDs/GUIDs where neccessary; \n",
		            " (2) Check/fix data entry errors in the rows provided below; \n",
					" (3) Remove duplicate rows programmatically; \n",
					" (4) Check any code chunks related to MERGING the data in this file. \n\n",
				"The following rows appear to be duplicates%s:\n\n%s\n\n\n\n"
			),
			x[["file_name"]],
		    length(x[["locations"]]),
			truncmess,
	        locs
		)
		class(message) <- c("invalid_report", "character")
		message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}


}

