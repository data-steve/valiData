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

    ## Note: This function used to test if there were additional columns of
    ## NAs with NA as column name that were created by a user manually inspecting
    ## a csv and accidentally clicking to the right of true columns and thus
    ## making new, unintended columns.  This used to be an error that readr did
    ## not parse differently from comma broken csv files.  readr now handles
    ## these types of csv file appropriately.

	offender_rows <- NULL
	cols  <- NULL
	proportion <- NULL

    data <- suppressWarnings(readr::read_csv(path))

    problem_cases <- readr::problems(data)

    ## The broken csv check works under the assumption that readr's `problem`
    ## function outputs a check with an expected and actual columns.  Broken
    ## csv files have a readr `problems` output that looks like this:

        ## > m <- readr::read_csv('Book1.csv')
        ## Warning: 2 parsing failures.
        ## row col  expected    actual
        ##   3  -- 6 columns 7 columns
        ##   5  -- 6 columns 1 columns

    ## Under the assumption that broken csv files will have a differening number
    ## expected and actual columns and that readr will report them in this
    ## fashion we can test for broken csv files by looking for `problems` output
    ## that have an expected column with the text "# columns" using the grep:
    ## "^\\d+\\s*columns$".
    ##
    ## This depends on readr always reporting broken csv files in this way.
    ##
    ## Known limitations:
    ##   1. This method will not catch broken csv files where there are
    ##      additional empty columns and the comma break pushes content into
    ##      these columns.

    ## First if/else is looking for (based on assumptions stated above):
    ##  1. readr `problems` function output problems w/ an 'expected' column
    ##  2. Does the 'expected' column contain the pattern "# columns"
    if (
            ## the readr problems has an 'expected' column
            !is.null(problem_cases[["expected"]]) &&

            ## the readr problems's expected column contains the word column
            any(grepl("^\\d+\\s*columns$", problem_cases[["expected"]]))
        ) {

            ## Check what elements of 'expected' column have "# columns" form
            offending_problems <- grepl("^\\d+\\s*columns$", problem_cases[["expected"]])
            ## Extract offending rows (index the grep from offending problems on the
            ## 'row' column from `problems` output)
            offender_rows <- problem_cases[["row"]][offending_problems]

            ## find any columns that could be considered character class
            cols <- candidates_for_quotes(path)
            valid <- FALSE
            error <- "empty-column"
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

    } else {

        cols <- NULL
        offender_rows <- NULL
        valid <- TRUE
        error <- NULL
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





