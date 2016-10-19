#' Validate a CSV File
#'
#' Validate a .csv file.
#'
#' @param path A path to a .csv file to be validated.
#' @param file_name name of file for which details need to be extracted from map.
#' @param map mapping of dictionary of data tests to be applied to import files.
#' @param \ldots ignored.
#' @return Prints a report to the console of tests for file type, comma broken
#' csv, number of columns, spaces in column headers, correct column names,
#' required columns containing values.
#' @export
validate_file <- function(path, file_name, map, ...){
# browser()
    ## check that file is csv
    file_type <- vf_file_type(path, type= map[["file_level"]][["type"]])

    if (file_type[["valid"]]) {

        ## check that file is not empty
        empty_file <- vf_non_empty(path)
        if (!empty_file[['valid']]) {
            obj <- list(file_level = list(empty_file = empty_file), table_level =NULL, column_level = NULL)
            class(obj) <- "validate_file"
            return(obj)
        }


        ## check that csv is not broken
        broken_csv <- vf_csv_broken(path)

        ##---------------##
        ## Check columns ##
        ##---------------##

        ## get data
        data <- suppressWarnings(readr::read_csv(path))
        colnames(data) <- gsub("^[^ -~]", "", colnames(data))  # put in to remove the <U+FEFF> character read_csv puts in first column header 8/15/2016

        ## converting broken rows from broken_csv to NA
        if (!broken_csv[["valid"]] && broken_csv[["error"]]=="comma-broken"){
            data[broken_csv[["locations"]][["rows"]]-1, ] <- NA
        }

        if (map[["table_level"]][["header"]]){
            header <- vt_header(data, map, file_name=file_name)
        } else {
            header <- NULL
        }

        ## grab space and case specifications from the map
        ignore_case <- map[["table_level"]][['ignore_case']]
        ignore_space <- map[["table_level"]][['spaced_columns']]

        ## check if column names have spaces
        if (!ignore_space){
            spaced_columns <- vt_spaced_colnames(data, file_name=file_name)
        } else {
            spaced_columns <- NULL
        }

        ## correct column names (using case and space specifications)
        column_names <- vt_column_names(
            data,
            map,
            file_name=file_name,
            ignore.case=ignore_case,
            ignore.space=ignore_space
        )

        if (map[["table_level"]][["non_empty"]]){
            non_empty <- vt_non_empty(data, map, file_name=file_name)
        } else {
            non_empty <- NULL
        }

        # testing for rows that are NA only
        na_only_rows <- vt_na_rows(data)

        ## if table is empty it stops here (no column level checks)
        if (!non_empty[["valid"]]) {
            obj <- list(
                file_level = list(file_type = file_type, empty_file = empty_file,
                    broken_csv = broken_csv),
                table_level = list(header = header, spaced_columns = spaced_columns,
                    column_names = column_names, non_empty = non_empty
                    , na_only_rows= na_only_rows),
                column_level = NULL,
                path = path,
                file_name = file_name
            )
            class(obj) <- "validate_file"
            return(obj)
        }

            ## This runs only if table was not empty (previous step)
            if (file_name %in% map[["table_level"]][["required_columns"]]){
                required_columns <- vt_required_columns(data, map, file_name=file_name)
            } else {
                required_columns <- NULL
            }

            if (map[["table_level"]][["column_order"]]){
                columns_order <- vt_column_order(data, map, file_name=file_name)
            } else {
                columns_order <- NULL
            }



            if (map[["table_level"]][["duplicate_rows"]]){
                duplicated_rows <- vt_duplicated_rows(data, file_name=file_name)
            } else {
                duplicated_rows <- NULL
            }

            ## check for nonASCII characters
            if (map[["table_level"]][["non_ASCII"]]){
                non_ASCII <- vt_non_ASCII(data, map, file_name=file_name)
            } else {
                non_ASCII <- NULL
            }

            if (non_ASCII[['valid']]|is.null(non_ASCII)) {
                data[] <- lapply(data, function(x) gsub("[[:cntrl:]]", "", suppressWarnings(stringi::stri_enc_toascii(x))))
            }


            ## column level testing
            columns_as_expected <- vc_column_apply(data, map[["column_level"]][[file_name]])

            obj <- list(
                file_level = list(empty_file = empty_file, broken_csv = broken_csv),
                table_level = list(header = header, spaced_columns = spaced_columns,
                    column_names = column_names, non_empty = non_empty,
                    required_columns = required_columns, duplicated_rows = duplicated_rows,
                    non_ASCII = non_ASCII, na_only_rows= na_only_rows),
                column_level = columns_as_expected,
                path = path,
                file_name = file_name
            )

            class(obj) <- "validate_file"
            return(obj)

    } else {

        obj <- list(
            file_level = list(file_type=file_type),
            table_level = NULL,
            column_level = NULL,
            path = path,
            file_name = file_name
        )
        class(obj) <- "validate_file"
        return(obj)
    }

}




#' Prints a validate_file Object
#'
#' Prints a validate_file object
#'
#' @param x A validate_file object.
#' @param \ldots ignored.
#' @method print validate_file
#' @export
print.validate_file <- function(x, ...){

    #print(header_file(x[['file_name']], x[['path']]))

    # file level
    file_all_valid <- assess_all_valid(x[['file_level']])
    if (!file_all_valid) cat(header("File Level Testing", char = "="))
    vector_print(x[['file_level']])


    ## table level
    table_all_valid <- assess_all_valid(x[['table_level']])
    if (!table_all_valid) cat(header("Table Level Testing", char = "="))
    vector_print(x[['table_level']])

    ## column level
    columns_all_valid <- assess_all_valid_multi(x[['column_level']])
    if (!columns_all_valid) cat(header("Column Level Testing", char = "="))
    vector_print(unlist(x[['column_level']], recursive=FALSE))


    ## cow say good if all columns valid
    if (file_all_valid && table_all_valid && columns_all_valid){
        print(report_all_is_well())
    }
}




assess_valid <- function(x, ...){
    is.null(x)|isTRUE(x[['valid']])
}


assess_all_valid <- function(x, ...){
    all(sapply(x, assess_valid))
}


assess_valid_multi <- function(x, ...){
    all(sapply(x, function(x) {
        isTRUE(x[['valid']])|is.null(x[['valid']])
    }))
}


assess_all_valid_multi <- function(x, ...){
    all(sapply(x, assess_valid_multi))
}


vector_print <- function(x, ...){
    invisible(lapply(x, function(y){
        if (is.null(y)) {
            cat("")
        } else {
            print(y)
        }
    }))
}
