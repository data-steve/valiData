#' Validate that a CSV Has No Missing/Null Values in Required Fields
#'
#' \code{vt_required_columns} - Validates that a .csv file's
#' \code{\link[base]{data.frame}} contains no missing/null values in required
#' fields.
#'
#' @param data \code{\link[base]{data.frame}}.
#' @param map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param missing Values considered to be missing or null.
#' @param prop.acceptable proportion of allowable missing
#' @param file_name An optional file name for use in reporting.
#' @return Returns a list of validation results.
#' @rdname vt_required_columns
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
#' vt_required_columns(df, map)
#' str(vt_required_columns(df, map))
vt_required_columns <- function(data, map, missing = c("", "NULL", "NA", "N/A", "na", "n/a"),
    prop.acceptable = 0, file_name = NULL){

    if (is.null(file_name)) file_name <- "The file"

    map_cols <- map[['table_level']][['required_columns']][[file_name]]
    required <- tolower(gsub("\\s+", "", map_cols))
	colnames(data) <- gsub("\\s+", "", tolower(colnames(data)))

    # don't assess columns unless present in both data and map
	required_columns <- data[, intersect(required, colnames(data)), drop=FALSE]

	# for campus labs the following is an artifact of bad mapping...knowing what's required
	# grep("[2-90]", required, value = TRUE, invert=TRUE)
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
				, file_name = file_name
				, stringsAsFactors = FALSE) -> required_df


	miss_cols <- required_df[c("cols", "required", "valid")]

    required_and_absent <- map_cols[!required %in% colnames(data)]

	list(cols =  map_cols[is.element(gsub("\\s+", "", tolower(map_cols)), names(required_columns))]
		 , valid = all(required_df[["valid"]]) & length(required_and_absent) == 0
		 , locations =  sort(unique(unlist(lapply(required_list, function(x) x[["locations"]] ) )))
		 , proportions = mean(required_df[["proportions"]])
		 , call = unique(required_df[["call"]])
		 , required = unique(required_df[["required"]])
		 , prop_acceptable = unique(required_df[["prop_acceptable"]])
		 , file_name = file_name
		 , n_cols_missing = sum(sapply(required_list, function(x) x[["proportion"]]) != 1)
		 , missing_columns = miss_cols[miss_cols[["required"]] & !miss_cols[["valid"]], "cols"]
	     , required_and_absent = required_and_absent
	) -> required_list


	req <- list(df = required_df, ls = required_list )
	class(req) <- 'vt_required_columns'
	req

}


#' Prints a vt_required_columns  Object
#'
#' Prints a vt_required_columns  object
#'
#' @param x A vt_required_columns  object.
#' @param \ldots ignored.
#' @method print vt_required_columns
#' @export
print.vt_required_columns <- function(ls, ...) {
	x <- ls[["ls"]]

	if (!isTRUE(x[["required_list"]][["valid"]])) {

	    if (length(x[["required_and_absent"]]) > 0){
	        missing_cols_message <- sprintf(
    			paste0(
    			    "'%s' does not contain the following required columns:\n\n%s\n\n",
    			    "This problem may be caused by misspelling column names or ommitting them from thefile."
    			),
			    x[["file_name"]],
     			paste(paste0("\t-", x[["required_and_absent"]]), collapse ="\n")
    		)
	    } else {
	        missing_cols_message <- ""
	    }

	    if (x[["n_cols_missing"]] > 0){
	        missing_vals_message <- sprintf(
    			paste0("'%s' contains the following %s required columns with missing/null values:\n\n%s\n\n",
    				"Required columns should not contain missing/null values; please edit the following rows: \n\n%s\n\n\n\n"
    			),
			    x[["file_name"]],
    			x[["n_cols_missing"]],
    			paste(paste0("\t- ", x[["missing_columns"]]), collapse = "\n"),
    			output_truncate(x[["locations"]])
    		)
	    } else {
	        missing_vals_message <- ""
	    }

		message <- paste0(header("Required Columns Test"),
		    missing_cols_message,
			missing_vals_message
		)

		class(message) <- c("invalid_report", "character")
		print(message)
	} else {
		message <- ""
		class(message) <- c("valid_report", "character")
		print(message)
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

