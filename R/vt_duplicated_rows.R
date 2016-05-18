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
#' str(vt_duplicated_rows(CO2[, 1:3]))
vt_duplicated_rows <- function(data, file.name = NULL) {

    . <- .GRP <- .N <- N <- .I <- GRP <- NULL
    dups <- NULL
    loc <- NULL
    dup_groups <- NULL

	if (is.null(file.name)) file.name <- "The file"

    not_missing <- rowSums(!t(apply(data, 1, is.na))) != 0
	dups <- duplicated(data) & not_missing  #added the part after '&' to exclude NA from dup testing 5/18/2016

	if (sum(dups)>0){

		prop <- sum(dups)/nrow(data)
		loc <- which(dups)

		data.table::as.data.table(data.table::copy(data))[, c("GRP", "N") := .(.GRP, .N), by = names(data)][
		    N > 1, list(list(.I)), by = GRP][["V1"]] %>%
		    {.[sapply(., function(x){
		        !all(x %in% which(!not_missing))
		        })]} %>%
		    sapply(function(x) paste0("(", paste(1 + x, collapse=","), ")", sep="")) %>%
		    paste(collapse=" ") -> dup_groups

	} else {

		prop <- NULL
		loc <- NULL
		dup_groups <- NULL
	}
	# browser()
	duprows <- list(
		valid = sum(dups) == 0,       ## logical did enough (proportion) elements validate
		locations = dup_groups,       ## location of those not validating
		# dup_groups = dup_groups,
		proportion = prop,            ## proportion of those vaidating
		call = "vt_duplicated_rows",  ## function name that was called
		file_name = file.name
	)
	class(duprows) <- 'vt_duplicated_rows'
	duprows

}



#' Prints a vt_duplicated_rows Object
#'
#' Prints a vt_duplicated_rows object
#'
#' @param x A vt_duplicated_rows object.
#' @param \ldots ignored.
#' @method print vt_duplicated_rows
#' @export
print.vt_duplicated_rows <- function(x, ...){

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
		print(message)
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		print(message)
	}


}

