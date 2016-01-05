#' Validates and Reports If Unique
#'
#' Validates and Reports If Unique
#'
#' @param x character vector
#' @param colname_x vector's colname
#' @export
vc_unique <- function(x,  colname_x = "the column"  ){

    x[(x %in% c("NULL", "NA", "N/A", "na", "n/a")) | grepl("^\\s*$", x)] <- NA

    is_unique <- duplicated(x)
    are_unique <- all(!is_unique|is.na(x))

    if (!are_unique){
        locs <- which(is_unique)

        message <- sprintf(
            "%s contains %s duplicates.\nThe following rows are duplicates:\n\n%s\n\n\n\n",
            sQuote(colname_x)
            , sum(is_unique)
            , output_truncate(locs))
        cat(message)

    }

    return(are_unique)
}


