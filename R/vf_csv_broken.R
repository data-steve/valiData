#' Validate that a CSV's Is Properly Quoted
#'
#' \code{vt_comma_broken} - Validates that a .csv file has properly quoted character
#' columns (is not a broken .csv).
#'
#' @param path Path to a .csv file.
#' @export
vf_csv_broken <- function(path){

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

    problem_cases <- readr::problems(data)

    if (!is.null(problem_cases[["expected"]])) {
        offending_problems <- grepl("^\\d+\\s*columns$", problem_cases[["expected"]])
        if (any(offending_problems)) {
	        offender_rows <- problem_cases[["row"]][offending_problems]
        }
    }

    if (nrow(problem_cases)>0) {
        offending_problems <- grepl("^\\d+\\s*columns$", problem_cases[["expected"]])

            if (nrow(problem_cases[offending_problems, ])>=nrow(data) ) {
                cols = NULL
                valid = FALSE
                error = "empty-column"
                offender_rows = 1:nrow(data)
                proportion = 1 - length(offender_rows)/nrow(data)
            } else {
                cols <- candidates_for_quotes(path)
                valid = FALSE
                error = "comma-broken"
                offender_rows = problem_cases[["row"]][offending_problems]
                proportion = 1 - length(offender_rows)/nrow(data)
            }
        } else {
            cols = NULL
            offender_rows = NULL
            error = NULL
            valid = TRUE
            proportion = 0
        }



	list(
		valid = valid,            ## logical did enough (proportion) elements validate
		locations = list(
			rows = offender_rows,        ## location of those not validating
			columns = cols
		),
		error=error,
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
broken_report <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {#{

	    if (x[["error"]]=="comma-broken"){
	        message <- sprintf( paste0(header("Broken CSV Test"),
               "The file '%s' appears to be a broken csv with about %d %% of rows being broken.\n",
               "Brokenness can affect the reliability of later tests.\n",
               "This is common when character columns are not quoted and exported to a .CSV.\n",
               "Please quote character columns in file '%s.'\n\n",
               "The following columns are potential unquoted character columns and may be the source of the break:\n\n%s\n\n",
               "Elements in the following rows appear to contain character values without quotes:\n\n%s\n\n\n\n"
        ),
        x[["file_name"]],
        round(x[["proportion"]]*100,0),
        x[["file_name"]],
        paste(paste0("\t- ", x[["locations"]][["columns"]]), collapse = "\n"),
        output_truncate(x[["locations"]][["rows"]])
    )
	    } else {
	        message <- sprintf(
	            paste0(header("Broken CSV Test"),
               "The file '%s' appears to be a broken csv by a trailing column being empty.\n",
               "Brokenness can affect the reliability of later tests.\n",
               "This is common when exported CSVs are manually inspected, introducing changes to original SQL pull.\n\n\n\n"
	        ),
	        x[["file_name"]]
	        )

	    }


	class(message) <- c("invalid_report", "character")
	message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}

}


