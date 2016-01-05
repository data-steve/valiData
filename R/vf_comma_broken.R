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

## Tyler updated b/c comma_broken was broken on 11/20/15
	# transitioned about from this regex-based test
	# because it wasn't catching everything 1/4/2016
# 	data <- stringi::stri_replace_all_regex(data, "(?<=(,|^))(\")(.*?)(\")(?=($|,))", "[PLACE HOLDER]")

#     regex_counts <- stringi::stri_count_regex(data, ",")
# 	offender_rows <- which(regex_counts > as.numeric(names(which.max(table(regex_counts)))))
    data <- suppressWarnings(readr::read_csv(path))

    problem_rows <- readr::problems(data)

    if (!is.null(problem_rows[["expected"]])) {
        offending_problems <- grepl("^\\d+\\s*columns$", problem_rows[["expected"]])
        if (any(offending_problems)) {
	        offender_rows <- problem_rows[["row"]][offending_problems]
        }
    }

	if (!is.null(offender_rows)) {
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

	data <- suppressWarnings(readr::read_csv(path, n_max = 30))
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
               "The file '%s' appears to be a comma broken csv with about %f %% being broken.",
               "Comma-broken status can affect the reliability of later tests.\n",
               "This is common when character columns are not quoted and exported to a .CSV.\n",
               "Please quote character columns in file '%s.'\n\n",
               "The following columns are potential unquoted character columns and may be the source of the break:\n\n%s\n\n",
               "Elements in the following rows appear to contain character values without quotes:\n\n%s\n\n\n\n"
        ),
        x[["file_name"]],
        x[["proportion"]],
        x[["file_name"]],
        paste(paste0("\t- ", x[["locations"]][["columns"]]), collapse = "\n"),
        output_truncate(x[["locations"]][["rows"]])
    )
	class(message) <- c("invalid_report", "character")
	message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}

}


