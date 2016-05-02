#' Checks For Null Elements In Vector
#'
#' Checks For Null Elements In Vector
#'
#' @param x char vector
#' @param prop.acceptable proportion of allowable null
#' @param null values null can take
#' @param required whether the vector in the table is required
#' @param file.name name of file containing the table of vectors
#' @export
#' @examples
#' set.seed(10)
#' x <- sample(c(NA, rep(LETTERS[1:10], 100)), 10000, TRUE)
#' vc_null(x)
vc_null <- function(x, prop.acceptable = 0, null = c("", "NULL"),
					required = FALSE, file.name = NULL){

	## make sure prop.acceptable is 0-1 range
	stopifnot(prop.acceptable >= 0 && prop.acceptable <= 1)

	## replace potential null values with NA
	x[x %in% null] <- NA

	## calculate the proportion null
	prop_null <- sum(is.na(x))/length(x)

	list(
		valid = prop_null <= prop.acceptable, ## logical did enough (proportion) elements validate
		locations = which(is.na(x))+1,                 ## location of those not validating
		proportion = 1 - prop_null,               ## proportion of those vaidating
		call = "vc_null",                          ## function name that was called
		required = required,
		prop_acceptable = prop.acceptable
	)
}
