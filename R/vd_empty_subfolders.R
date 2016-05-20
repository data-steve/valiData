#' Validate Which Folders Contain CSVs
#'
#' Validate Which Folders Contain CSVs
#'
#' @param path path to project directory
#' @param \dots ignored.
#' @return Returns a list of validation results.
#' @rdname vd_empty_subfolders
#' @export
#' @examples
#' dir_name <- file.path(tempdir(), "delete_me")
#' dir.create(dir_name)
#' dir(dir_name)
#' vd_empty_subfolders(dir_name)
#'
#' sub_dirs <- file.path(dir_name, c('courses', 'students', 'faculty'))
#' lapply(sub_dirs, dir.create)
#' lapply(sub_dirs, dir.exists)
#' write.csv(mtcars, file=file.path(sub_dirs[3], 'mtcars.csv'))
#' dir(dir_name, recursive=TRUE, include.dirs =  TRUE, full.names = TRUE)
#' dir(dir_name, include.dirs =  TRUE, full.names = TRUE)
#' vd_empty_subfolders(dir(dir_name, include.dirs =  TRUE, full.names = TRUE))
vd_empty_subfolders <- function(path, ...){
	. <- NULL

	empties <- !sapply(get_paths_root_to_files(path), function(x) {
	    length(dir(x)) > 0 | any(tools::file_ext(dir(x)) %in% 'csv')
	})

	are_valid <- sum(empties) == 0

	if (!are_valid){
		message <-  paste0(
		    header("Empty Folders Notice", char="~"),
		    "The following folders did not contain any CSVs to test:\n\n",
		    paste(paste0("\t -", names(which(empties))) , collapse = "\n" ),
		    "\n\n\n\n"
		)
	} else {
	    message <- NULL
	}
	report <- list(
		valid = are_valid,                    ## logical did enough (proportion) elements validate
		locations = names(which(empties)),    ## location of those not validating,
	    message = message,
		call = "vd_empty_subfolders"          ## function name that was
	)
	class(report) <- "vd_empty_subfolders"
	report
}


#' Prints a vd_empty_subfolders Object
#'
#' Prints a vd_empty_subfolders object
#'
#' @param x A vd_empty_subfolders object.
#' @param \ldots ignored.
#' @method print vd_empty_subfolders
#' @export
print.vd_empty_subfolders <- function (x, ...)	{

	if (!isTRUE(x[["valid"]])) {
		cat(x[['message']])
	}
}
