#' Checks If File Not Empty
#'
#' Checks If File Not Empty
#'
#' @param path path to file
#' @export
#' @rdname vf_non_empty
#' @examples
#' writeLines("", '~/Desktop/filename.csv')
#' 		vf_non_empty('~/Desktop/filename.csv') 		#  FALSE
#' writeLines("hi", '~/Desktop/filename1.csv')
#' 		vf_non_empty('~/Desktop/filename1.csv') 	#  TRUE

vf_non_empty <- function(path, ...){

	if (length(readLines(path),n=10) == 0L) {    
		message <- sprintf(
			paste0(
			    header("File Empty Test"),
				"'%s' does not have any contents.\n",
				"Please either remove from upload\nor ensure required content is included.\n\n"
			),
			basename(path)
		)
    } else {
        message <- ''
    }

	non_empty <- list(
		valid = length(readLines(path),n=10) != 0L,  ## logical did enough (proportion) elements validate
		call = "vf_non_empty",                      ## function name that was called
		file_name = basename(path),
	    message = message
	)

	class(non_empty) <- 'vf_non_empty'
	non_empty
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