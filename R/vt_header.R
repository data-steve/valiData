#' Validate that a CSV Has a Header
#'
#' \code{vt_header} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} contains no missing/null values in required
#' fields.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param file.name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_header
#' @export
#' @examples
#' set.seed(10)
#' map <- data.frame(
#'     header = colnames(mtcars),
#'     required = sample(c(TRUE, FALSE), ncol(mtcars), TRUE), stringsAsFactors = FALSE
#' )
#'
#' df <- mtcars; colnames(df) <- mtcars[1, ]
#' vt_header(df, map)
#' header_report(vt_header(df, map))
vt_header <- function(data, map, file.name = NULL){

    if (is.null(file.name)) file.name <- "The file"

    list(
        valid = sum(colnames(data) %in% map[["header"]]) > 0,  ## logical did enough (proportion) elements validate
        locations = NULL,                        ## location of those not validating
        call = "vt_header",                        ## function name that was called
        file_name = file.name,
        expected_header = map[["header"]],
        actual_header = colnames(data)
    )

}


#' Validate that a CSV's has N Columns
#'
#' \code{header_report} - Generates accomanying report.
#'
#' @param x A file or table validation function's (prefixed with \code{vf_} or
#' \code{vt_}) output.
#' @param \ldots ignored.
#' @rdname vt_header
#' @export
header_report <- function(x, ...){

    if (!isTRUE(x[["valid"]])) {

        headers <- paste(sQuote(x[["actual_header"]]), collapse=", ")
        if (nchar(headers) > 100) headers <- paste0(gsub(", '[^']+$", "", substring(headers, 1, 90)), ", ...[truncated]...")

        message <- sprintf(
            paste0(header("Contains a Header Test"),
                   "'%s' does not appear to have a header. These were the header names:\n\n",
                   headers,
                   "\n\n",
                   "Either the file:\n",
                   " (1) Has no header -or-\n",
                   " (2) All file names are misspelled. \n\n",
                   "*Note: not all tests could be run due to missing header.\n\n\n\n"
            ),
            x[["file_name"]]
        )

        class(message) <- c("invalid_report", "character")
        message
    } else {
        message <- ""
        class(message) <- c("valid_report", "character")
        message
    }

}
