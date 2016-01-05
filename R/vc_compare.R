#' Validates and Reports If Comparison Met
#'
#' Validates and Reports If Comparison Met
#'
#' @param x character vector to be coerced
#' @param y character vector to be coerced and compared
#' @param comparison logical operator for the comparison
#' @param colname_x X vector's colname
#' @param colname_y Y vector's colname
#' @param date logical.  If \code{TRUE} x and y are converted to dates via
#' \code{parsedate::parse_iso_8601}.
#' @param \ldots ignored.
#' @export
vc_compare <- function(x, y, comparison, colname_x = "the X column" ,
    colname_y = "the Y column", date = FALSE, ...){

    if(missing(x)) (return())
    if(missing(y)) (return())

    x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA
    y[(y %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", y)] <- NA

    original_na <- is.na(x)|is.na(y)

    if (date) {
        x[!is.na(x)] <- parsedate::parse_iso_8601(trimws(x[!is.na(x)]))
        y[!is.na(y)] <- parsedate::parse_iso_8601(trimws(y[!is.na(y)]))
    }
    if (all(is.na(x))|all(is.na(y))) {
        message <- sprintf("The date formats used in either %s or %s or both do not follow the ISO 8601 required.\n\n\n\n", colname_x, colname_y)
        cat(message)
    } else {


        is_compare <- compare(x, y, comparison)
        are_compare <- all(is_compare|original_na)



        if (!are_compare ){
            message <- sprintf(
                "The following rows of %s fail \nbecause they are %s %s:\n\n%s\n\n\n\n"
                , sQuote(colname_x)
                , switch(comparison
                         , "==" = "not equal to"
                         , "!=" = "equal to"
                         , ">"  = "not greater than"
                         , "<"  = "not less than"
                         , ">=" = "not greater than or equal to"
                         , "<=" = "not less than or equal to")
                , sQuote(colname_y)
                , paste(which(!is_compare & !original_na )+1
                        ,collapse=", "))
            cat(message)

        }

        return(are_compare)
        }
}

#' Main Helper Function for vc_compare
#'
#' Helper Function for vc_compare
#'
#' @param xx character vector to be coerced
#' @param yx character vector to be coerced and compared
#' @param fun logical operator
#' @export
compare <- function(xx, yy, fun){
    match.fun(fun)(xx, yy)
}
