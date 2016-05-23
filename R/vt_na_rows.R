#' Find Rows with only NAs, If Any
#'
#' Find Rows with only NAs, If Any.
#'
#' @param data the data
#' @param file_name the file name
#' @export
#' @example
#' #' set.seed(10)
#' test <- data.frame(matrix(rep(NA,ncol(mtcars)*2), nrow=2))
#' test2 <- rbind(mtcars, stats::setNames( test, names(mtcars) ))
#'
#' vt_na_rows(test2)
#' str(vt_na_rows(test2))
vt_na_rows <- function(data, file_name=NULL){

    if (is.null(file_name)) file_name <- "The file"

    na_rows <- list(
        valid = !any(rowSums(is.na(data))/ncol(data) == 1),
        locations = which(rowSums(is.na(data))/ncol(data) == 1, useNames = F),               ## location of those not validating
        call = "vt_na_rows",                        ## function name that was called
        file_name = file_name
    )
    class(na_rows) <- 'vt_na_rows'
    na_rows

}


#' Prints a vt_na_rows Object
#'
#' Prints a vt_na_rows object
#'
#' @param x A vt_na_rows object.
#' @param \ldots ignored.
#' @method print vt_na_rows
#' @export
print.vt_na_rows <- function(x, ...){

    if (!isTRUE(x[["valid"]])) {

        message <- sprintf(
            paste0(header("Test for Rows with All NAs"),
                   "'%s' appear to be rows containing only NAs for their values:\n\n",
                   output_truncate(x[["locations"]]),
                   "\n\n",
                   "Find these rows and delete them.\n\n\n\n"
            ),
            x[["file_name"]]
        )

        class(message) <- c("invalid_report", "character")
        print(message)
    } else {
        message <- ""
        class(message) <- c("valid_report", "character")
        print(message)
    }

}
