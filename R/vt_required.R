#' Validate that a CSV Has No Missing/Null Values in Required Fields
#'
#' \code{vt_required} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} contains no missing/null values in required
#' fields.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param missing Values considered to be missing or null.
#' @param prop.acceptable proportion of allowable missing
#' @param file.name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_required
#' @export
#' @examples
#' set.seed(10)
#' map <- data.frame(
#'     header = colnames(mtcars),
#'     required = sample(c(TRUE, FALSE), ncol(mtcars), TRUE), stringsAsFactors = FALSE
#' )
#'
#' df <- mtcars
#' df[c(1, 4), c("drat", "cyl")] <- NA
#' vt_required(df, map)
#' required_report(vt_required(df, map))
vt_required <- function(data, map, missing = c("", "NULL", "NA", "N/A", "na", "n/a"), prop.acceptable = 0, file.name = NULL){

    if (is.null(file.name)) file.name <- "The file"
    stopifnot(all(c("header", "required") %in% colnames(map)))
    prop.acceptable <- 0

    map_cols <- map[["header"]]
	colnames(data) <- gsub("\\s+", "", tolower(colnames(data)))
	map[["header"]] <- gsub("\\s+", "", tolower(map_cols))
	#map[["required"]][is.na(map[["required"]])] <- "FALSE"  # maybe remove once map gets improved

	required <- map[['header']][as.logical(map[["required"]])]
	map_cols <- map_cols[as.logical(map[["required"]])]
	#fix the map because Tyler should never no never have hard-code a crutch instead of really fixing this problem

    # don't assess columns unless present in both data and map
	required_columns <- data[, intersect(required, colnames(data)), drop=FALSE]

	# for campus labs the following is an artifact of bad mapping...knowing what's required
	# grep("[2-90]", required, value = TRUE, invert=TRUE)
 # browser()
	required_list <- invisible(lapply(required_columns, vc_non_response,
		prop.acceptable = prop.acceptable, missing = missing, required = TRUE))


	# Dear Future DS_subjugate,
	# We (Steve-n-Tyler) added unique to defining cols below because CrossListing import
	# allows for SectionIdentifier to show up multiple times
	# all the test for valid, location, proportions, ... thus only treat SectionIdentifier once
	# so we get more cols obs than for other vars 1/4/16 Love, data_steve
	data.frame( cols = unique(map_cols[is.element(gsub("\\s+", "", tolower(map_cols)), names(required_columns))])
				, valid = sapply(required_list, function(x) x[["valid"]])
				, locations = sapply(required_list, function(x) paste(x[["locations"]], collapse=", ") )  # should we change this by removing paste?
				, proportions = sapply(required_list, function(x) x[["proportion"]])
				, call = sapply(required_list, function(x) x[["call"]])
				, required = sapply(required_list, function(x) x[["required"]])
				, prop_acceptable = sapply(required_list, function(x) x[["prop_acceptable"]])
				, file_name = file.name
				, stringsAsFactors = FALSE) -> required_df


	miss_cols <- required_df[c("cols", "required", "valid")]

	list(cols =  map_cols[is.element(gsub("\\s+", "", tolower(map_cols)), names(required_columns))]
		 , valid = all(required_df[["valid"]])
		 , locations =  sort(unique(unlist(lapply(required_list, function(x) x[["locations"]] ) )))
		 , proportions = mean(required_df[["proportions"]])
		 , call = unique(required_df[["call"]])
		 , required = unique(required_df[["required"]])
		 , prop_acceptable = unique(required_df[["prop_acceptable"]])
		 , file_name = file.name
		 , n_cols_missing = sum(sapply(required_list, function(x) x[["proportion"]]) != 1)
		 , missing_columns = miss_cols[miss_cols[["required"]] & !miss_cols[["valid"]], "cols"]
	) -> required_list

	return(list(df = required_df, ls = required_list ))

}


#' Validate that a CSV Has No Missing/Null Values in Required Fields
#'
#' \code{required_report} - Generates accomanying report.
#'
#' @param ls A file or table validation function's (prefixed with \code{vf_} or
#' \code{vt_}) output.
#' @param \ldots ignored.
#' @rdname vt_required
#' @export
required_report <- function(ls, ...) {
	x <- ls[["ls"]]

	if (!isTRUE(x[["valid"]])) {
		message <- sprintf(
			paste0(header("Required Columns Test"),
				"'%s' contains the following %s required columns with missing/null values:\n\n%s\n\n",
				"Required columns should not contain missing/null values; please edit the following rows: \n\n%s\n\n\n\n"
			),
			x[["file_name"]],
			x[["n_cols_missing"]],
			paste(paste0("\t- ", x[["missing_columns"]]), collapse = "\n"),
			output_truncate(x[["locations"]])
		)

		class(message) <- c("invalid_report", "character")
		message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}

}

## NOT currently used...
## may be used in future iterations
# required_report_df <- function(x, ...){
# 	x <- x[["df"]]
#
# 	Map(function(y, z){
# 		mess <- missing_helper_ls(y)
# 		if (is.null(mess)) {
# 			message <- sprintf(mess, z, z)
# 			class(message) <- c("invalid_report", "character")
# 			message
# 		}  else {
# 			message <- ""
# 			class(message) <- c("valid_report", "character")
# 			message
# 		} } , x, names(x))
#
# }


missing_helper_df <- function(x, ...){

	if (!isTRUE(x[["valid"]])) {

		message <-sprintf(
				paste0(
					"'%%s' contains %s%% missing/null values.  This is %s a required column.\n",
		            "It is expected that this column will have no more than %s%%%% values missing/null.\n",
					"The following row numbers contained missing/null values.\n",
					"Please edit these elements as necessary;\n\n%s\n\n\n\n"
				),
				round(100*(1-x[["proportion"]]), 1),
				ifelse(x[["required"]], "", "not "),
				round(100*(x[["prop_acceptable"]]), 1),
				output_truncate(paste0("\t- ", x[["locations"]]))
	)
		class(message) <- c("invalid_report", "character")
		message
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		message
	}

}

