#' Validates If Comparison Met
#'
#' Validates If Comparison Met
#'
#' @param data A data frame.
#' @param x Column name from \code{data} (character string).
#' @param y Column name from \code{data} (character string) to be compared to.
#' @param comparison logical operator for the comparison
#' @param date logical.  If \code{TRUE} x and y are converted to dates via
#' \code{parsedate::parse_iso_8601}.
#' @param \ldots ignored.
#' @export
#' @examples
#' dat <- data.frame(
#'     a = c(NA, 1:10),
#'     b = c(0, 1, 2:10+1),
#'     d1 = c(NA, sprintf("2016-01-%sT09", 11:20)),
#'     d2 = c("2016-01-01T09", "2016-01-11T09", sprintf("2016-05-%sT09", 12:20)),
#'     stringsAsFactors = FALSE
#' )
#'
#' vc_compare(dat, x = 'a', y = 'b', '<')
#' vc_compare(dat, x = 'a', y = 'b', '<=')
#' vc_compare(dat, x = 'd1', y = 'd2', '<', date=TRUE)
#' vc_compare(dat, x = 'd1', y = 'd2', '<=', date=TRUE)
vc_compare <- function(data, x, y, comparison, date = FALSE, ...){

    # if(missing(x)) (return())
    # if(missing(y)) (return())

    ## select the column & replace missing with NA
    colx <- sub_out_missing(data[[x]])
    coly <- sub_out_missing(data[[y]])

    ## record missing (NA)
    is_na <- c(is.na(colx))|c(is.na(coly))

    if (isTRUE(date)) {
        colx[!is.na(colx)] <- parsedate::parse_iso_8601(trimws(colx[!is.na(colx)]))
        coly[!is.na(coly)] <- parsedate::parse_iso_8601(trimws(coly[!is.na(coly)]))
    }

    if (all(is.na(colx))|all(is.na(coly))) {
        message <- sprintf("The date formats used in either %s or %s or both do not follow the ISO 8601 required.\n\n\n\n", x, y)
        is_valid <- rep(FALSE, length(colx))
        are_valid <- FALSE
    } else {

        ## expression to validate against (elementwise)
    	is_valid <- compare(colx, coly, comparison)

    	## valid columnwise: Are all elelemnts either valid or NA?
    	are_valid <- all(is_valid|is_na)

    	## generate the comment
    	if (!are_valid){
            message <- sprintf(
                "The following rows of %s are not valid \nbecause they are %s %s:\n\n%s\n\n\n\n",
                    sQuote(x),
                    switch(comparison,
                        "==" = "not equal to",
                        "!=" = "equal to",
                        ">"  = "not greater than",
                        "<"  = "not less than",
                        ">=" = "not greater than or equal to",
                        "<=" = "not less than or equal to",
                        "~=" = "not almost equal (enough)",
                        "invalid `compare` argument"
                    ),
                    sQuote(y),
                    output_truncate(which(!(is_valid|is_na)))
            )
    	} else {
    	    message <- NULL
    	}

    }

    ## construct vc list & class
    vc_output <- list(
        column_name = x,
        valid = are_valid,
        message = message,
        passing = is_valid,
        missing = is_na,
        call = 'vc_compare'
    )

    class(vc_output) <- 'vc'
    vc_output

}

#' Nearly Equal
#'
#' Helper Function for vc_compare to implement all.equal as new comparison operator
#'
#' @param x number one
#' @param y number two
#' @export
`~=` <- function(x,y){
  isTRUE(all.equal(x, y))
}

#' Main Helper Function for vc_compare
#'
#' Helper Function for vc_compare
#'
#' @param xx character vector to be coerced
#' @param yy character vector to be coerced and compared
#' @param fun logical operator
#' @export
compare <- function(xx, yy, fun){
    match.fun(fun)(xx, yy)
}
