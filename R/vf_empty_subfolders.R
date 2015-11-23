#' Validate Which Folders Contain CSVs
#'
#' Validate Which Folders Contain CSVs
#'
#' @param all_subpaths absolute paths from root to all subfolders under path, see documentation for \code{get_paths_root_to_files(path)}
#' @param csv_subpaths paths to all subfolders CONTAINING CSVs, see documentation for \code{get_paths_to_csvs(path)}
#' @return Returns a list of validation results.
#' @rdname vf_empty_subfolders
#' @export

vf_empty_subfolders <- function(all_subpaths, csv_subpaths){
	. <- NULL
	
	empties <- setdiff(all_subpaths
					   , sapply(csv_subpaths , function(x) {paste(strsplit(x, "/")[[1]] %>%
					   										   	.[1:length(.)-1] , collapse="/")} )
	)
	
	list(
		valid = length(empties) == 0,      ## logical did enough (proportion) elements validate
		locations = empties,                  ## location of those not validating,
		call = "vf_empty_subfolders"         ## function name that was
	)
}


#' @param x output of \code{vf_empty_subfolders()}
#' @param \ldots extra parameters
#' @rdname vf_empty_subfolders
#' @export
report_empty_subfolders <- function (x, ...)	{
	
	if (!isTRUE(x[["valid"]])) {
		message <-  paste0(header("Empty Folders Notice", char="~"),
						   "The following folders did not contain any CSVs to test:\n"
						   , paste(paste0("\t -", x[["locations"]]) , collapse = "\n" )
						   , "\n\n\n\n"
		)
		class(message) <- c("invalid_report", "character")
		message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}
}
