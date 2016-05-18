#' Validate that a CSV's Is Properly Quoted
#'
#' \code{vf_csv_broken} - Validates that a .csv file has properly quoted character
#' columns (is not a broken .csv).
#'
#' @param path Path to a .csv file.
#' @param \dots ignored.
#' @export
#' @rdname vf_csv_broken
#' @examples
#' loc <- file.path(tempdir(), 'temp.csv')
#' cat(
#'     paste(c(",x,y", "1, the dog, wen,1", "2,door,2"), collapse="\n"),
#'     file=loc
#' )
#' vf_csv_broken(loc)
vf_csv_broken <- function(path, ...){

	offender_rows <- NULL
	cols  <- NULL
	proportion <- NULL

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

                valid <- FALSE
                error <- "empty-column"
                offender_rows <- 1:nrow(data)
                proportion <- 1 - length(offender_rows)/nrow(data)

    	        message <- sprintf(
    	            paste0(
        	           header("Broken CSV Test"),
                       "The file '%s' appears to be a broken csv by a trailing column being empty.\n",
                       "Brokenness can affect the reliability of later tests.\n",
                       "This is common when exported CSVs are manually inspected, introducing changes to original SQL pull.\n\n\n\n"
    	            ),
    	            basename(path)
    	        )


            } else {

                cols <- candidates_for_quotes(path)
                valid <- FALSE
                error <- "comma-broken"
                offender_rows <- problem_cases[["row"]][offending_problems]
                proportion <- 1 - length(offender_rows)/nrow(data)

    	        message <- sprintf(
        	            paste0(header("Broken CSV Test"),
                       "The file '%s' appears to be a broken csv with about %d%% of rows being broken.\n",
                       "Brokenness can affect the reliability of later tests.\n",
                       "This is common when character columns are not quoted and then exported to a .CSV.\n",
                       "Please quote character columns in file '%s.'\n\n",
                       "The following columns are potential unquoted character columns and may be the source of the break:\n\n%s\n\n",
                       "Elements in the following rows appear to contain character values without quotes:\n\n%s\n\n\n\n"
                    ),
                    basename(path),
                    round(proportion*100,0),
                    basename(path),
                    paste(paste0("\t- ", cols), collapse = "\n"),
                    output_truncate(offender_rows)
                )

            }
        } else {

            cols <- NULL
            offender_rows <- NULL
            error <- NULL
            valid <- TRUE
            proportion <- 0
            message <- ""
        }



	broken <- list(
		valid = valid,                   ## logical did enough (proportion) elements validate
		locations = list(
			rows = offender_rows,        ## location of those not validating
			columns = cols
		),
		error=error,
		proportion = proportion,          ## proportion of those vaidating
		call = "vf_csv_broken",           ## function name that was called
		file_name = basename(path),       ## file name
	    message = message
	)
	class(broken) <- 'vf_csv_broken'
	broken

}



candidates_for_quotes <- function(path) {

	data <- suppressWarnings(readr::read_csv(path, n_max = 30))
	names(data[sapply(data, class) ==  "character"])

}


#' Prints a vf_csv_broken Object
#'
#' Prints a vf_csv_broken object
#'
#' @param x A vf_csv_broken object.
#' @param \ldots ignored.
#' @method print vf_csv_broken
#' @export
print.vf_csv_broken <- function (x, ...)	{
	if (!isTRUE(x[["valid"]])) {
		cat(x[['message']])
	}
}

# #' Validate that a CSV's Is Properly Quoted
# #'
# #' \code{comma_broken_report} - Generates accomanying report.
# #'
# #' @param x A file or table validation function's (prefixed with \code{vf_} or
# #' \code{vt_}) output.
# #' @param \ldots ignored.
# #' @rdname vf_csv_broken
# #' @export
#
#
#
#
# broken_message <- function(x, ...){
#
# 	if (!isTRUE(x[["valid"]])) {#{
#
# 	    if (x[["error"]]=="comma-broken"){
# 	        message <- sprintf( paste0(header("Broken CSV Test"),
#                "The file '%s' appears to be a broken csv with about %d %% of rows being broken.\n",
#                "Brokenness can affect the reliability of later tests.\n",
#                "This is common when character columns are not quoted and exported to a .CSV.\n",
#                "Please quote character columns in file '%s.'\n\n",
#                "The following columns are potential unquoted character columns and may be the source of the break:\n\n%s\n\n",
#                "Elements in the following rows appear to contain character values without quotes:\n\n%s\n\n\n\n"
#         ),
#         x[["file_name"]],
#         round(x[["proportion"]]*100,0),
#         x[["file_name"]],
#         paste(paste0("\t- ", x[["locations"]][["columns"]]), collapse = "\n"),
#         output_truncate(x[["locations"]][["rows"]])
#     )
# 	    } else {
# 	        message <- sprintf(
# 	            paste0(header("Broken CSV Test"),
#                "The file '%s' appears to be a broken csv by a trailing column being empty.\n",
#                "Brokenness can affect the reliability of later tests.\n",
#                "This is common when exported CSVs are manually inspected, introducing changes to original SQL pull.\n\n\n\n"
# 	        ),
# 	        x[["file_name"]]
# 	        )
#
# 	    }
#
# 	} else {
# 		message <- ""
# 	}
#
#     message
# }


