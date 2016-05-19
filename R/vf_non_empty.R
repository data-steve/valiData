#' Checks If File Not Empty
#'
#' Checks If File Not Empty
#'
#' @param path path to file
#' @export
#' @rdname vf_non_empty
#' @examples
#' writeLines("", '~/filename.csv')
#' 		vf_non_empty('~/filename.csv') 		#  FALSE
#' writeLines("hi", '~/filename1.csv')
#' 		vf_non_empty('~/filename1.csv') 	#  TRUE

vf_non_empty <- function(path, ...){

if (length(readLines(path),n=10) == 0L) {
		message <- sprintf(
			paste0(
			    header("File Empty Test"),
				"'%s' does not have any content.\n",
				"Please either remove from upload\nor ensure required content is included.\n\n"
			),
			basename(path)
		)
    } else {
        message <- ''
    }

	file_type <- list(
		valid = length(readLines(path),n=10) != 0L,  ## logical did enough (proportion) elements validate
		call = "vf_non_empty",                      ## function name that was called
		file_name = basename(path),
	    message = message
	)

	class(file_type) <- 'vf_non_empty'
	file_type
}


#' Prints a vf_non_empty Object
#'
#' Prints a vf_non_empty object
#'
#' @param x A vf_non_empty object.
#' @param \ldots ignored.
#' @method print vf_non_empty
#' @export
print.vf_non_empty <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {
		cat(x[['message']])
	}

}
