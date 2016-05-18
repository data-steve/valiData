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

        if (map[["table_level"]][["header"]]){
            header <- vt_header(data, map, file.name=file_name)
        } else {
            header <- NULL
        }

        if (map[["table_level"]][["spaced_columns"]]){
            spaced_columns <- vt_spaced_columns(data, map, file.name=file_name)
        } else {
            spaced_columns <- NULL
        }

        if (ignore_case){
            if (map[["table_level"]][["column_names"]]){
                column_names <- vt_column_names(data, map, file.name=file_name, ignore.case=TRUE)
            } else {
                column_names <- NULL
            }
        } else {
            if (map[["table_level"]][["column_names"]]){
                column_names <- vt_column_names(data, map, file.name=file_name)
            } else {
                column_names <- NULL
            }
        }


        ## grab info for column names ignoring case and space
        if (!column_names[["valid"]]){
            column_names_ignore_case_space <- vt_column_names(data, map, file.name=file_name, ignore.case=TRUE, ignore.space=TRUE)
        }


        if (map[["table_level"]][["non_empty"]]){
            non_empty <- vt_non_empty(data, map, file.name=file_name)
        } else {
            non_empty <- NULL
        }
        if (non_empty[["valid"]]) {
            return(list(broken_csv, header, spaced_colums, column_names, non_empty))
        }


        if (map[["table_level"]][["required_columns"]]){
            required_columns <- vt_required_columns(data, map, file.name=file_name)
        } else {
            required_columns <- NULL
        }

        if (map[["table_level"]][["columns_order"]]){
            columns_order <- vt_columns_order(data, map, file.name=file_name)
        } else {
            columns_order <- NULL
        }



        if (map[["table_level"]][["duplicated_rows"]]){
            duplicated_rows <- vt_duplicated_rows(data, map, file.name=file_name)
        } else {
            duplicated_rows <- NULL
        }

        # check for nonASCII characters
        if (map[["table_level"]][["non_ASCII"]]){
            non_ASCII <- vt_non_ASCII(data, map, file.name=file_name)
        } else {
            non_ASCII <- NULL
        }

        if (non_ASCII) {
            data[] <- lapply(data, function(x) gsub("[[:cntrl:]]", "", suppressWarnings(stringi::stri_enc_toascii(x))))
        }

        #-------------------------------------# column level mapping [START] added by Tyler 11/20/15

        cat(header("Column-Wise Testing"))
        columns_as_expected <- all(unlist(vc_column_apply(data, map[["column_level"]])))

        #xxx
        if (columns_as_expected) {
            cat("All columns meet expectations!\n\n\n\n")
        }
        #------------------------------------# column level mapping [END] added by Tyler 11/20/15

        list(file_type, header, spaced_columns, column_names,
             column_names_ignore_case_space, non_empty, required_columns,
             column_order, duplicated_rows, non_ASCII, columns_as_expected)
        }

    } else {

        list(file_type)
    }

}
