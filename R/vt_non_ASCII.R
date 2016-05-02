#' Validates and Reports If Non-ASCII characters present
#'
#' Validates and Reports If Non-ASCII characters present
#'
#' @param data the data to be tested
#' @param \ldots ignored
#' @export
vt_non_ASCII <- function(data, ...){

    is_non_ASCII <- sapply(data, function(x) any(grepl("[[:cntrl:]]", suppressWarnings(stringi::stri_enc_toascii(x)))))

    if (any(is_non_ASCII)){
        which_hv_nonASCII <- names(is_non_ASCII[is_non_ASCII])
        where_is_nonASCII <- lapply(data[which_hv_nonASCII], function(x) grep("[[:cntrl:]]", suppressWarnings(stringi::stri_enc_toascii(x)) ))
        message <- paste0(
            "The following rows of "
            , sQuote(which_hv_nonASCII)
            , " contain non-ASCII characters:\n\n"
            ,  sapply(where_is_nonASCII, output_truncate)
            , "\n\n")
        cat(message)
    }


    return(invisible(any(is_non_ASCII)))
}
