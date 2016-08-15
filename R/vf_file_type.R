#' Checks If File CSV
#'
#' Checks If File CSV
#'
#' @param file path to file
#' @param type extension of file representing type
#' @export
#' @rdname vf_file_type
#' @examples
#' vf_file_type("broken_csv.R")           #  FALSE
#' vf_file_type("broken_csv.R", type="R") #  TRUE
#' vf_file_type("hello.csv")              #  TRUE
vf_file_type <- function(file, type= "csv") {

	if (tools::file_ext(file) != type) {
		message <- sprintf(
			paste0(
			    header("File Type Test"),
				"'%s' does not have the expected .%s file extension.\n",
				"It appears to be a .%s file extension.\n\n",
				"Please upload a .%s file type.\n\n\n\n"
			),
			basename(file),
			type,
			tools::file_ext(file),
			type
		)
    } else {
        message <- ''
    }

	file_type <- list(
		valid = tools::file_ext(file) == type,  ## logical did enough (proportion) elements validate
		call = "vf_file_type",                      ## function name that was called
		file_extension = tools::file_ext(file),     ## function name that was called
		file_name = basename(file),
		type = type,
	    message = message
	)

	class(file_type) <- 'vf_file_type'
	file_type
}

#' Prints a vf_file_type Object
#'
#' Prints a vf_file_type object
#'
#' @param x A vf_file_type object.
#' @param \ldots ignored.
#' @method print vf_file_type
#' @export
print.vf_file_type <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {
		cat(x[['message']])
	}

}
