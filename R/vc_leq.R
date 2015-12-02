#' Validates and Reports If Less Than Or Equal To
#'
#' Validates and Reports If Less Than Or Equal To
#'
#' @param x character vector to be coerced
#' @param y character vector to be coerced and compared
#' @param colname_x X vector's colname
#' @param colname_y Y vector's colname

vc_leq <- function(x, y, colname_x = "the X column" , colname_y = "the Y column"){

    x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA

    is_leq <- x < y
    are_leq <- all(is_leq|is.na(x))

    if (!are_leq ){
        message <- sprintf(
            "The following rows of %s are not less than or equal to %s:\n\n%s\n\n\n\n",
            sQuote(colname_x)
            , sQuote(colname_y)
            , paste(which(!is_leq )+1
                    ,collapse=", "))
        cat(message)

    }

    return(are_leq)
}
