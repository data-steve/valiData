#' Prints a vc Object
#'
#' Prints a vc object
#'
#' @param x A vc object.
#' @param \ldots ignored.
#' @method print vc
#' @export
print.vc <- function(x, ...){
    if(is.null(x[["message"]])) return(invisible(NULL))
    cat(x[["message"]])
}
