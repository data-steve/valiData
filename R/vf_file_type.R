#' Checks If File CSV
#' 
#' Checks If File CSV
#' 
#' @param file path to file
#' @param type extension of file representing type

#' @rdname vf_empty_subfolders
#' @export
#' @examples 
#' x <- vf_file_type("broken_csv.R")           #  FALSE
#' x <- vf_file_type("broken_csv.R", type="R") #  TRUE
#' x <- vf_file_type("hello.csv")              #  TRUE
#' report_file_type(x) 
vf_file_type <- function(file, type= "csv") {
	list(
		valid = tools::file_ext(file) == type,  ## logical did enough (proportion) elements validate
		locations = NULL,                           ## location of those not validating,
		proportion = NULL,                          ## proportion of those vaidating
		call = "vf_file_type",                      ## function name that was called
		file_extension = tools::file_ext(file),     ## function name that was called
		file_name = basename(file),
		type = type
	)
}

#' @param x output of \code{vf_file_type()}
#' @param \ldots extra parameters
#' @rdname vf_empty_subfolders
#' @export
report_file_type <- function(x, ...){
	
	if (!isTRUE(x[["valid"]])) {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
		
		
	} else {
		message <- sprintf(
			paste0(header("File Type = CSV Test"),
				   "'%s' does not have the expected .%s file extension.\n",
				   "It appears to be a .%s file extension.\n\n",
				   "Please upload a .%s file type."
			),
			x[["file_name"]],
			x[["type"]],
			x[["file_extension"]],
			x[["type"]]
		)
		class(message) <- c("invalid_report", "character")
		message
	}
	
	
}
