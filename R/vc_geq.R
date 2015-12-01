#' Validates and Reports If Greater Than Or Equal To
#'
#' Validates and Reports If Greater Than Or Equal To
#'
#' @param x character vector to be coerced
#' @param y character vector to be coerced and compared
#' @param colname_x X vector's colname
#' @param colname_y Y vector's colname

vc_geq <- function(x, y, colname_x = "the X column" , colname_y = "the Y column"){

    x[x %in% c("", "NULL", "NA", "N/A", "na", "n/a")] <- NA

    is_geq <- x >= y
    are_geq <- all(is_geq|is.na(x))

    if (!are_geq ){
        message <- sprintf(
            "The following rows of %s are not greater than or equal to %s:\n\n%s\n\n\n\n",
            sQuote(colname_x)
            , sQuote(colname_y)
            , paste(which(!is_geq )
                    ,collapse=", "))
        cat(message)

    }

    return(are_geq)

}
