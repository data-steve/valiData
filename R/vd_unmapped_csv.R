#' Validate Folders that Contain CSVs Are Found in Map
#'
#' Validate Folders that contain CSVs are found in map
#'
#' @param paths paths to folders containing mapped csv files.
#' @param map A \pkg{valiData} map.
#' @param \dots ignored.
#' @return Returns a list of validation results.
#' @rdname vd_unmapped_csv
#' @export
vd_unmapped_csv <- function(paths, map, ...){
	. <- NULL

    folder_names <- tolower(basename(dirname(paths)))
    map_file_names <- names(map[["column_level"]])
    non_terminatng_subfolder_with_csv <- !folder_names %in% map_file_names

	are_valid <- sum(non_terminatng_subfolder_with_csv) == 0

	if (!are_valid){
		message <-  paste0(
		    header("Un-Mapped Subfolder with CSV Notice", char="~"),
		    "The following folders contained CSVs but the parent folder is NOT found in the map:\n\n",
		    paste(paste0("\t -", paths[non_terminatng_subfolder_with_csv]) , collapse = "\n" ),
		    "\n\n\n\nNo validation was attempted on this file(s).\nPlease place in mapped folders or remove this csv file(s).",
		    "\n\n\n\n"
		)
	} else {
	    message <- NULL
	}
	report <- list(
		valid = are_valid,                    ## logical did enough (proportion) elements validate
		locations = paths[non_terminatng_subfolder_with_csv],    ## location of those not validating,
	    logical_locations = non_terminatng_subfolder_with_csv,
	    message = message,
		call = "vd_unmapped_csv"          ## function name that was
	)
	class(report) <- "vd_unmapped_csv"
	report
}


#' Prints a vd_unmapped_csv Object
#'
#' Prints a vd_unmapped_csv object
#'
#' @param x A vd_unmapped_csv object.
#' @param \ldots ignored.
#' @method print vd_unmapped_csv
#' @export
print.vd_unmapped_csv <- function (x, ...)	{

	if (!isTRUE(x[["valid"]])) {
		cat(x[['message']])
	}
}
