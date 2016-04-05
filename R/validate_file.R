#' Validate a CSV File
#'
#' Validate a .csv file.
#'
#' @param file A path to a .csv file to be validated.
#' @param map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param \ldots ignored.
#' @return Prints a report to the console of tests for file type, comma broken
#' csv, number of columns, spaces in column headers, correct column names,
#' required columns containing values.
#' @export
#' @examples
#' \dontrun{
#' validate_file("C:/Users/trinker/Desktop/myfolder/totest.csv", core_data_map[["totest"]])
#' }
validate_file <- function(file, core_data_map, column_map,...){

    #on.exit(suppressWarnings(sink()))
	## check that file is csv
	file_type <- vf_file_type(file)

	if (file_type[["valid"]]) {

		## check that csv is not broken
		broken_csv <- vf_csv_broken(file)
		print(broken_report(broken_csv))


		## Check columns

		data <- suppressWarnings(readr::read_csv(file))

		## converting broken rows from broken_csv to NA
		if (!broken_csv[["valid"]] && broken_csv[["error"]]=="comma-broken"){


		    data[broken_csv[["locations"]][["rows"]]-1, ] <- NA
		    #data <- data[which(!seq_len(nrow(data)) %in%  broken_csv[["locations"]][["rows"]]),]
		}

		base_file <- basename(file)
		has_header <- vt_header(data, core_data_map, file.name=base_file)
        if (!has_header[["valid"]]) {
            print(header_report(has_header))
            return(invisible())
        }

		enough_cols <- vt_ncols(data, core_data_map, file.name=base_file)
		spaced_colnames <- vt_spaced_colnames(data, file.name=base_file)
		correct_colnames  <- vt_colnames(data, core_data_map, file.name=base_file, ignore.case=TRUE)
		non_empty <- vt_non_empty(data, file.name=base_file)  ## added non_empty report 11/20/15 Tyler
		print(non_empty_report(non_empty))                         ## added non_empty report 11/20/15 Tyler
		if (!non_empty[["valid"]]) return()


		has_required_cols <- vt_required(data, core_data_map, file.name=base_file)
	    #column_order <- vt_colorder(data, core_data_map, file.name=base_file)

		## grab info for column names ignoring case and space
		if (!correct_colnames[["valid"]]){
			correct_colnames_ignore_case_space <- vt_colnames(data, core_data_map, file.name=base_file, ignore.case=TRUE, ignore.space=TRUE)
		}

		# print reports

		# decide between print 4 or 5 reports based on whether
		print(ncols_report(enough_cols))
		print(colnames_report(correct_colnames))
		print(spaced_colnames_report(spaced_colnames))
		print(required_report(has_required_cols))

		## check for duplicated rows
		data <- data[rowSums(!t(apply(data, 1, is.na))) != 0, ]

		duplicated_rows <- vt_duplicated_rows(data, file.name=base_file)
		print(duplicated_rows_report(duplicated_rows))

		# check for nonASCII characters
		non_ASCII <- vt_non_ASCII(data)
		if (non_ASCII) {
		    data[] <- lapply(data, function(x) gsub("[[:cntrl:]]", "", suppressWarnings(stringi::stri_enc_toascii(x))))
		}

        #-------------------------------------# column level mapping [START] added by Tyler 11/20/15

		cat(header("Column-Wise Testing"))
        columns_as_expected <- all(unlist(vc_column_apply(data, column_map)))

#xxx
        if (columns_as_expected) {
            cat("All columns meet expectations!\n\n\n\n")
        }
        #------------------------------------# column level mapping [END] added by Tyler 11/20/15

		if (duplicated_rows[["valid"]] && enough_cols[["valid"]] &&
				correct_colnames[["valid"]] && has_required_cols[["ls"]][["valid"]] &&
				broken_csv[["valid"]] && columns_as_expected && !non_ASCII){   # tyler added `columns_as_expected` for vc_ tests on 11/20/15
		    print(report_all_is_well())
		}

	} else {

		# report on not csv
		report_file_type(file_type)
	}

}


# validate_file <- function(file, core_data_map, ...){  #depracated on 11/20/15
#
# 	## check that file is csv
# 	file_type <- vf_file_type(file)
#
# 	if (file_type[["valid"]]) {
#
# 		## check that csv is not broken
# 		broken_csv <- vf_comma_broken(file)
# 		print(report_comma_broken(broken_csv))
#
#
# 		## Check columns
# 		data <- read_csv_character(file)
#
# 		base_file <- basename(file)
# 		enough_cols <- vt_ncols(data, core_data_map, file.name=base_file)
# 		spaced_colnames <- vt_spaced_colnames(data, file.name=base_file)
# 		correct_colnames  <- vt_colnames(data, core_data_map, file.name=base_file, ignore.case=TRUE)
# 		has_required_cols <- vt_required(data, core_data_map, file.name=base_file)
# 	    #column_order <- vt_colorder(data, core_data_map, file.name=base_file)
#
# 		## grab info for column names ignoring case and space
# 		if (!correct_colnames[["valid"]]){
# 			correct_colnames_ignore_case_space <- vt_colnames(data, core_data_map, file.name=base_file, ignore.case=TRUE, ignore.space=TRUE)
# 		}
#
# 		# print reports
#
# 		# decide between print 4 or 5 reports based on whether
# 		print(ncols_report(enough_cols))
# 		print(colnames_report(correct_colnames))
# 		print(spaced_colnames_report(spaced_colnames))
# 		print(required_report(has_required_cols))
#
# 		## check for duplicated rows
# 		duplicated_rows <- vt_duplicated_rows(data, file.name=base_file)
# 		print(report_duplicated_rows(duplicated_rows))
#
# 		if (duplicated_rows[["valid"]] && enough_cols[["valid"]] &&
# 				correct_colnames[["valid"]] && has_required_cols[["ls"]][["valid"]] &&
# 				broken_csv[["valid"]]){
# 		    print(report_all_is_well())
# 		}
#
# 	} else {
#
# 		# report on not csv
# 		report_file_type(file_type)
# 	}
#
# }


