#' Checks If Path Is Directory
#'
#' Checks If Path Is Directory
#'
#' @param path path in directory
#' @param \ldots ignored.
#' @export
vd_dir <- function(path, ...) {  #not currently used/checked in valiData (should this be added?)
	list(
		valid = file.info(path)[["isdir"]],
		locations = NULL,
		call = "vd_dir",
		path = basename(path)
	)
}
