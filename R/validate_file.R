#' Validate a CSV File
#'
#' Validate a .csv file.
#'
#' @param file A path to a .csv file to be validated.
#' @param core_data_map A \code{data.frame} with a \code{header} (header name) &
#' \code{required} (logical,; Is column required?).
#' @param column_map A \code{data.frame} with a \code{header} (header name) &
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
validate_file <- function(path, file_name="academic", map=test,...){

    ## check that file is csv
    file_type <- vf_file_type(path, type= map[["file_level"]][["type"]])

    if (file_type[["valid"]]) {

        ## check that file is not empty
        empty_file <- vf_non_empty(path)
        if (!empty_file[['valid']]) {
            obj <- list(empty_file = empty_file)
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
        if (non_empty[["valid"]]) {
            obj <- list(broken_csv = broken_csv, header = header,
                spaced_columns = spaced_columns, column_names = column_names,
                non_empty = non_empty
            )
            class(obj) <- "validate_file"
            return(obj)
        }

            ## This runs only if table was not empty (previous step)
            if (map[["table_level"]][["required_columns"]]){
                required_columns <- vt_required_columns(data, map, file_name=file_name)
            } else {
                required_columns <- NULL
            }

            if (map[["table_level"]][["columns_order"]]){
                columns_order <- vt_columns_order(data, map, file_name=file_name)
            } else {
                columns_order <- NULL
            }



            if (map[["table_level"]][["duplicated_rows"]]){
                duplicated_rows <- vt_duplicated_rows(data, map, file_name=file_name)
            } else {
                duplicated_rows <- NULL
            }

            # check for nonASCII characters
            if (map[["table_level"]][["non_ASCII"]]){
                non_ASCII <- vt_non_ASCII(data, map, file_name=file_name)
            } else {
                non_ASCII <- NULL
            }

            if (non_ASCII) {
                data[] <- lapply(data, function(x) gsub("[[:cntrl:]]", "", suppressWarnings(stringi::stri_enc_toascii(x))))
            }

            ## column level testing
            columns_as_expected <- vc_column_apply(data, map[["column_level"]][[file_name]])

            obj <- list(file_type = file_type, header = header,
                spaced_columns = spaced_columns, column_names = column_names,
                non_empty = non_empty, required_columns = required_columns,
                column_order = column_order, duplicated_rows = duplicated_rows,
                non_ASCII = non_ASCII, columns_as_expected = columns_as_expected
            )
            class(obj) <- "validate_file"
            return(obj)

    } else {

        obj <- list(file_type=file_type)
        class(obj) <- "validate_file"
        return(obj)
    }

}



