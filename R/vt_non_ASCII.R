#' Validates and Reports If Non-ASCII characters present
#'
#' Validates and Reports If Non-ASCII characters present
#'
#' @param data the data to be tested
#' @param \ldots ignored
#' @export
#' @examples
#' dat <- data.frame(
#'     txt = c(
#'         "fa\xE7ile test of showNonASCII():",
#'         "\\details{",
#'         "   This is a good line",
#'         "   This has an \xfcmlaut in it.",
#'         "   OK again.",
#'         "}"
#'     ),
#'     x = 1:6,
#'     stringsAsFactors = FALSE
#' )
#' vt_non_ASCII(dat)
#' str(vt_non_ASCII(dat))
vt_non_ASCII <- function(data, ...){

    is_non_ASCII <- sapply(data, function(x) any(grepl("[[:cntrl:]]", suppressWarnings(stringi::stri_enc_toascii(x)))))

    if (any(is_non_ASCII)){
        which_hv_nonASCII <- names(is_non_ASCII[is_non_ASCII])
        where_is_nonASCII <- lapply(data[which_hv_nonASCII], function(x) grep("[[:cntrl:]]", suppressWarnings(stringi::stri_enc_toascii(x)) ))
        message <- paste0(header("Non-ASCII Test"),
            "The following rows of "
            , sQuote(which_hv_nonASCII)
            , " contain non-ASCII characters:\n\n"
            ,  sapply(where_is_nonASCII, output_truncate)
            , "\n\n")
    } else {
        message <- NULL
    }

    nonascii <- list(
        valid = !any(is_non_ASCII),
        message = message,
        passing = is_non_ASCII
    )
    class(nonascii) <- 'vt_non_ASCII'

    nonascii
}



#' Prints a vt_non_ASCII  Object
#'
#' Prints a vt_non_ASCII  object
#'
#' @param x A vt_non_ASCII  object.
#' @param \ldots ignored.
#' @method print vt_non_ASCII
#' @export
print.vt_non_ASCII <- function(x, ...){
    if (!isTRUE(x[['valid']])){
        cat(x[['message']])
    }
}
