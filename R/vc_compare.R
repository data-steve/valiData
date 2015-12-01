#' Validates and Reports If Comparison Met
#'
#' Validates and Reports If Comparison Met
#'
#' @param x character vector to be coerced
#' @param y character vector to be coerced and compared
#' @param comparison logical operator for the comparison
#' @param colname_x X vector's colname
#' @param colname_y Y vector's colname
#' @export
vc_compare <- function(x, y, comparison, colname_x = "the X column" , colname_y = "the Y column", ...){

    x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA

    is_compare <- compare(x, y, comparison)
    are_compare <- all(is_compare|is.na(x))


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
            , paste(which(!is_compare )
                    ,collapse=", "))
        cat(message)

    }

    return(are_compare)

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
