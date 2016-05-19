#' Title
#' 
#' Description
#' 
#' @param d
#' @export
#' @examples 
cleaner <-
function (d) {
    colnames(d) <- tolower(colnames(d))
    d <- d[!sapply(names(d), function(x) all(is.na(d[x])), USE.NAMES = FALSE)]
    d[!rowSums(is.na(d))/ncol(d) == 1, ]
} 

