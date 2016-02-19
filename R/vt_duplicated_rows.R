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
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @examples
#' vt_duplicated_rows(CO2)
#' vt_duplicated_rows(CO2[, 1:3])
#' duplicated_rows_report(vt_duplicated_rows(CO2[, 1:3]))
vt_duplicated_rows <- function(data, file.name = NULL) {

    . <- NULL
    dups <- NULL
    loc <- NULL
    dup_groups <- NULL

	if (is.null(file.name)) file.name <- "The file"

	dups <- duplicated(data)

	if (sum(dups)>0){

		prop <- sum(dups)/nrow(data)
		loc <- which(dups)

		data.table::as.data.table(data.table::copy(data))[, c("GRP", "N") := .(.GRP, .N), by = names(data)][
		    N > 1, list(list(.I)), by = GRP][["V1"]] %>%
		    sapply(function(x) paste0("(", paste(x, collapse=","), ")", sep="")) %>%
		    paste(collapse=" ") -> dup_groups

# - Added data.table reporting of duplicate rows because dplyr approach couldn't
#   handle duplicate column names:
#		http://stackoverflow.com/questions/34314490/match-group-duplicate-rows-indices
#
# 		dplyr::add_rownames(data) %>%
# 		    setNames(paste0(colnames(.), 1:ncol(.))) %>%
# 		    dplyr::group_by_(.dots= names(.)) %>%
# 		    dplyr::filter(n()>1) %>%
# 		    dplyr::summarise(rn= paste(rowname, collapse=", ")) %>%
# 		    .$rn %>%
# 		    paste0("(", ., ")", collapse="") -> dup_groups

	} else {

		prop <- NULL
		loc <- NULL
		dup_groups <- NULL
	}
	# browser()
	list(
		valid = sum(dups) == 0,                          ## logical did enough (proportion) elements validate
		locations = dup_groups,    ## location of those not validating
		# dup_groups = dup_groups,
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

	    dups <- strsplit(trimws(gsub("[(]|[)]|,|s+", " ", x[["locations"]])), "\\s+")[[1]]
		dup_len <- length(dups)
		if (dup_len > 100) {
			locs <- paste0(paste(substring(x[["locations"]],1,100), collapse=", "), "...[truncated]...")
			truncmess <- " (truncated to first 100 elements)"
		} else {
			truncmess <- ""
			locs <- paste(x[["locations"]], collapse=", ")
		}
		message <- sprintf(
			paste0(#header("Duplicated Rows Test"),
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
			dup_len,
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

