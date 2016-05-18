validate_file <- function(path, file_name="academic", map=test,...){
    # core_data_map, column_map
    # path , file_name , file_level
    ## check that file is csv
    file_type <- vf_file_type(path, type= map[["file_level"]][["type"]])

    if (file_type[["valid"]]) {

        ## check that csv is not broken
        broken_csv <- vf_csv_broken(path)



        ## Check columns

        data <- suppressWarnings(readr::read_csv(path))

        ## converting broken rows from broken_csv to NA
        if (!broken_csv[["valid"]] && broken_csv[["error"]]=="comma-broken"){
            data[broken_csv[["locations"]][["rows"]]-1, ] <- NA
        }

        has_header <- vt_header(data, file.name=file_name, map)
         if (map[["table_level"]][["enough_cols"]]){
             enough_cols <- vt_enough_cols(data, map, file.name=file_name)
         } else {
             enough_cols <- NULL
         }

        spaced_colnames <- vt_spaced_colnames(data, file.name=base_file)
        correct_colnames  <- vt_colnames(data, core_data_map, file.name=base_file, ignore.case=TRUE)
        non_empty <- vt_non_empty(data, file.name=base_file)  ## added non_empty report 11/20/15 Tyler
        print(non_empty_report(non_empty))                         ## added non_empty report 11/20/15 Tyler
        if (!non_empty[["valid"]]) return()


        has_required_cols <- vt_required(data, core_data_map, file.name=base_file)
        column_order <- vt_colorder(data, core_data_map, file.name=base_file)

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
