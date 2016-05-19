#' Cleans Incoming Data from Excel
#'
#' Cleans incoming data a lot of invisible NAs can get imputed in Excel that need to be removed
#'
#' @param d data to be cleaned
#' @export
#' @examples
#' \dontrun{
#' ll <- read_csv("~/Desktop/file.csv")
#' ll <- cleaner(ll)
#' }
cleaner <- function (d) {
    colnames(d) <- tolower(colnames(d))
    d <- d[!sapply(names(d), function(x) all(is.na(d[x])), USE.NAMES = FALSE)]
    d[!rowSums(is.na(d))/ncol(d) == 1, ]
}


