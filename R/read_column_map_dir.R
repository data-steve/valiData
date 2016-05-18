#' Read in Debian Control File
#'
#' Read in Debian Control file column mapping.
#'
#' @param path Path to directory containing multiple Debian Control files.
#' @param names Option vector of names to add to the list output.  Defualt uses
#' the name of the Debian Control files sans file extenson.
#' @export
#' @return Returns a list of named column tests to be run in \code{valiData}.
read_column_map_dir <- function(path, names=NULL) {
    files <- dir(path, full.names = TRUE)

    col_test_maps <- stats::setNames(invisible(lapply(files, read.dcf)), tools::file_path_sans_ext(dir(path)))

    col_test_maps <- lapply(col_test_maps, function(x){
        x <- x[, sapply(x, function(y) y != "")]
        stats::setNames(lapply(strsplit(x, "\\;"), function(y) gsub(", \\)", ")", sub("\\(", "(x, ", y))), names(x))
    })

    if (!is.null(names) & length(col_test_maps) == length(names)) {
        col_test_maps <- stats::setNames(col_test_maps, names)
    }
    if (!is.null(names) & length(col_test_maps) != length(names)) {
        warning(sprintf("Number of `names` not equal to number of files in %s.", path))
    }
    col_test_maps
}


