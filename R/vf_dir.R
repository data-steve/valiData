#' Checks If Path Is Directory
#' 
#' Checks If Path Is Directory
#' 
#' @param path path in directory
#' @export
vf_dir <- function(path) {
	file.info(path)[["isdir"]]
	
	list(
		valid = file.info(path)[["isdir"]],   ## logical did enough (proportion) elements validate
		locations = NULL,                         ## location of those not validating,
		proportion = NULL,                        ## proportion of those vaidating
		call = "vf_dir",                          ## function name that was called
		file_name = basename(path)                ## file name
	)
}