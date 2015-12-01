#' Validate that a CSV's Is Properly Quoted
#'
#' \code{vt_comma_broken} - Validates that a .csv file has properly quoted character
#' columns (is not a broken .csv).
#'
#' @param path Path to a .csv file.
#' @export
vf_comma_broken <- function(path){

	offender_rows <- NULL
	cols  <- NULL
	proportion <- NULL

# 	# if (vf_file_type(path)) {
# 	data <- readLines(path)
# browser()
# m <- readr::read_csv(path)
# data[287]
# # readr::read_csv(path)
#     ## Tyler updated b/c comma_broken was broken on 11/20/15
# 	data <- stringi::stri_replace_all_regex(data, "(?<=(,|^))(\")(.*?)(\")(?=($|,))", "[PLACE HOLDER]")
# data[287]
#     regex_counts <- stringi::stri_count_regex(data, ",")
# 	offender_rows <- which(regex_counts > as.numeric(names(which.max(table(regex_counts)))))
    data <- suppressWarnings(readr::read_csv(path))
	offender_rows <- attributes(data)[["problems"]][["row"]]

# 	y <- stringi::stri_count_regex(data, ",")
# 	offender_rows <- which(y > as.numeric(names(which.max(table(y)))))


	if (length(offender_rows) > 0) {
		cols <- candidates_for_quotes(path)
		valid <- FALSE
		proportion <- 1 - length(offender_rows)/length(data)
	} else {
		valid <- TRUE
	}

	list(
		valid = valid,            ## logical did enough (proportion) elements validate
		locations = list(
			rows = offender_rows,        ## location of those not validating
			columns = cols
		),
		proportion = proportion,          ## proportion of those vaidating
		call = "vf_comma_broken",         ## function name that was called
		file_name = basename(path)        ## file name
	)

}



candidates_for_quotes <- function(path) {

	data <- read.csv(path, stringsAsFactors = FALSE, nrows = 30, header=TRUE)
	names(data[sapply(data, class) ==  "character"])

}


#' Validate that a CSV's Is Properly Quoted
#'
#' \code{comma_broken_report} - Generates accomanying report.
#'
#' @param x A file or table validation function's (prefixed with \code{vf_} or
#' \code{vt_}) output.
#' @param \ldots ignored.
#' @rdname vt_comma_broken
#' @export
comma_broken_report <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {#{

	message <- sprintf(
		paste0(header("Broken CSV Test"),
			"'%s' appears to be a comma broken csv.",
			"Comma-broken status can affect the reliability of later tests.\n",
			"This is common when character columns are not quoted and exported to a .CSV.\n",
			"Please quote character columns in %s.\n\n",
			"The following columns are potential unquoted character columns and may be the source of the break:\n\n%s\n\n",
			"Elements in the following rows appear to contain character values without quotes:\n\n%s\n\n\n\n"
		),
		x[["file_name"]],
		x[["file_name"]],
		paste(paste0("\t- ", x[["locations"]][["columns"]]), collapse = "\n"),
		paste(x[["locations"]][["rows"]], collapse = ", ")
	)
	class(message) <- c("invalid_report", "character")
	message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}

}


