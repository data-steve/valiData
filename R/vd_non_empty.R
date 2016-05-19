#' Checks If Dir Not Empty
#'
#' Checks If Dir Not Empty
#'
#' @param path path to Dir
#' @export
#' @rdname vd_non_empty
#' @examples
#' dir.create("~/totally_rad_man")
#' 	  vd_non_empty("~/totally_rad_man") 	#  FALSE
#' writeLines("hi", '~/filename1.csv')
#' 	  vd_non_empty('~/filename1.csv') 	    #  TRUE

vd_non_empty <- function(path, ...){

	if (length(dir(path))==0L) {
		message <- sprintf(
			paste0(
			    header("Directory Empty Test"),
				"'%s' does not have any files/folders.\n",
				"No further tests can be run until data files are included.\n"
				"Please retry once content is included.\n\n"
			),
			dirname(path)
		)
    } else {
        message <- ''
    }

	non_empty <- list(
		valid = length(dir(path))==0L,  ## logical did enough (proportion) elements validate
		call = "vd_non_empty",                      ## function name that was called
		file_name = dirname(path),
	    message = message
	)

	class(non_empty) <- 'vd_non_empty'
	non_empty
}


#' Prints a vd_non_empty Object
#'
#' Prints a vd_non_empty object
#'
#' @param x A vd_non_empty object.
#' @param \ldots ignored.
#' @method print vd_non_empty
#' @export
print.vd_non_empty <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {
		cat(x[['message']])
	}

}
