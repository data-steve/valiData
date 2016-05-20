#' Checks For Non-Response Elements In Vector
#'
#' Checks For Non-Response Elements In Vector
#'
#' @param x char vector
#' @param prop.acceptable proportion of allowable non-reponse
#' @param missing values non-response can take
#' @param required whether the vector in the table is required
#' @param file_name name of file containing the table of vectors
#' @export
#' @examples
#' set.seed(10)
#' x <- sample(c(NA, rep(LETTERS[1:10], 100)), 10000, TRUE)
#' vc_non_response(x)
vc_non_response <- function(x, prop.acceptable = 0,
							missing = c("", "NULL", "NA", "N/A", "na", "n/a"), required = FALSE, file_name = NULL){

	## make sure prop.acceptable is 0-1 range
	stopifnot(prop.acceptable >= 0 && prop.acceptable <= 1)

	## replace potential missing values with NA
	x[x %in% missing] <- NA

	## calculate the proportion missing
	prop_missing <- sum(is.na(x))/length(x)

	list(
		valid = prop_missing <= prop.acceptable, ## logical did enough (proportion) elements validate
		locations = which(is.na(x))+1,                 ## location of those not validating
		proportion = 1 - prop_missing,               ## proportion of those vaidating
		call = "vc_non_response",                     ## function name that was called
		required = required,
		prop_acceptable = prop.acceptable
	)
}
