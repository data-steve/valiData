#' Checks If Path Is Directory
#'
#' Checks If Path Is Directory
#'
#' @param path path in directory
#' @param \ldots ignored.
#' @export
#' vd_dir(system.file())
#' vd_dir(system.file("INDEX"))
vd_dir <- function(path, ...) {
	out <- list(
		valid = file.info(path)[["isdir"]],
	    message = paste0(
	        "The following path is not a dirrectory:\n\n\t-",
            path,
            "\n\nCurrently, `valiData` operates on a directory with files -OR- a ",
            "directory of sub-directories containing files\n\n\n\n"
        ),
		locations = NULL,
		call = "vd_dir",
		path = basename(path)
	)
	class(out) <- 'vd_dir'
	out
}



#' Prints a vd_dir Object
#'
#' Prints a vd_dir object
#'
#' @param x A vd_dir object.
#' @param \ldots ignored.
#' @method print vd_dir
#' @export
print.vd_dir <- function (x, ...)	{

	if (!isTRUE(x[["valid"]])) {
		cat(x[['message']])
	}
}
