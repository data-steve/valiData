#' Prints a valid_report Object
#'
#' Prints a valid_report object
#'
#' @param x A \code{valid_report} object.
#' @param \ldots ignored.
#' @method print valid_report
#' @export
print.valid_report <- function(x, ...){
		cat("")
}

#' Prints a all_good Object
#'
#' Prints a all_good object
#'
#' @param x An \code{all_good} object.
#' @param \ldots ignored.
#' @method print all_good
#' @export
print.all_good <- function(x, ...){
		cat(x)
}

#' Prints a invalid_report Object
#'
#' Prints a invalid_report object
#'
#' @param x An \code{invalid_report} object.
#' @param \ldots ignored.
#' @method print invalid_report
#' @export
print.invalid_report <- function(x, ...){
	if (!is.null(x)){
		cat(x)
	}
}
